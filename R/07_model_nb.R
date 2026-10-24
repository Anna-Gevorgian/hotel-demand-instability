# R/07_model_nb.R
# ---------------------------------------------------------------
# Stage 7: Negative Binomial regression on weekly cancellation
# counts.  THIS IS THE PROJECT'S HEADLINE STATISTICAL MODEL.
#
# Model: weekly_cancellations ~ behavioral_mix + context
#        + hotel + offset(log(weekly_bookings))
#
# Answers RQ2: what drives weekly cancellation volatility?
#
# We also fit a Poisson model for comparison so we can show
# the dispersion problem empirically (the LRT comparison
# between NB and Poisson via theta is the cleanest defense).
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(MASS)        # glm.nb
  library(broom)
  library(here)
  library(ggplot2)
})
source(here("R", "utils", "plotting.R"))

set.seed(2026)

weekly <- readRDS(here("data", "processed", "weekly_agg.rds")) |>
  mutate(
    hotel             = factor(hotel),
    event_active_week = as.integer(event_active_week)
  )

# ---- Train/test split: temporal --------------------------------
# Training: bookings made before 2017-01-01
# Test:     bookings made on/after 2017-01-01
train <- weekly |> filter(booking_week <  as.Date("2017-01-01"))
test  <- weekly |> filter(booking_week >= as.Date("2017-01-01"))

cat("\n--- Split sizes ---\n")
cat("Train weeks×hotels:", nrow(train), "\n")
cat("Test  weeks×hotels:", nrow(test),  "\n")

# ---- Model formula ---------------------------------------------
# Note the offset: log(weekly_bookings) means we model the RATE
# of cancellation per booking, not the raw count.
fml <- weekly_cancellations ~
  weekly_avg_lead_time +
  weekly_share_OTA +
  weekly_share_corporate +
  weekly_share_groups +
  weekly_holiday_density +
  event_active_week +
  hotel +
  offset(log(weekly_bookings))

# ---- Fit Negative Binomial -------------------------------------
nb_model <- glm.nb(fml, data = train)
cat("\n--- Negative Binomial summary ---\n")
print(summary(nb_model))
cat("\nNB theta (dispersion):", round(nb_model$theta, 3),
    " SE:", round(nb_model$SE.theta, 3), "\n")
cat("(Small theta = high week-to-week unpredictability)\n")

# ---- Fit Poisson for comparison --------------------------------
poisson_model <- glm(fml, family = poisson(), data = train)

# Likelihood-ratio test: is NB significantly better than Poisson?
# NB has one extra parameter (theta), so we compare 2*(logL_NB - logL_Pois)
# against chi-square(1).
lr_stat <- 2 * (logLik(nb_model) - logLik(poisson_model))
lr_pval <- pchisq(as.numeric(lr_stat), df = 1, lower.tail = FALSE)
cat("\n--- NB vs Poisson LRT ---\n")
cat("LR statistic:", round(as.numeric(lr_stat), 2), "\n")
cat("p-value     :", format.pval(lr_pval), "\n")
cat("(p < 0.001 confirms overdispersion: NB is correct over Poisson)\n")

# Pearson dispersion under Poisson (rule of thumb: ≈ 1 is good fit)
pearson_disp <- sum(residuals(poisson_model, type = "pearson")^2) /
  poisson_model$df.residual
cat("Pearson dispersion under Poisson:",
    round(pearson_disp, 2),
    " (>>1 ⇒ overdispersion confirmed)\n")

# ---- Incidence Rate Ratios with 95% CIs ------------------------
irrs <- tidy(nb_model, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term     = stringr::str_replace_all(term, "_", " "),
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high= round(conf.high, 3)
  )

cat("\n--- IRRs (exp coefficients), 95% CI ---\n")
print(irrs)

# Forest plot of IRRs
p_irr <- plot_forest(
  irrs |> dplyr::select(term, estimate, conf.low, conf.high),
  log_x  = TRUE,
  title  = "Negative Binomial: incidence rate ratios for weekly cancellation",
  x_label = "IRR (95% CI), log scale"
)
ggsave(here("report", "figures", "F08_nb_irr_forest.png"),
       p_irr, width = 7, height = 4.5, dpi = 200)

# ---- Predicted vs observed on held-out test --------------------
test$pred <- predict(nb_model, newdata = test, type = "response")
rmse <- sqrt(mean((test$weekly_cancellations - test$pred)^2))
mae  <- mean(abs(test$weekly_cancellations - test$pred))
cat("\n--- Held-out test performance ---\n")
cat("RMSE:", round(rmse, 2), "  MAE:", round(mae, 2), "\n")

# Observed-vs-predicted time series
test_plot_df <- test |>
  arrange(hotel, booking_week)

p_pred <- ggplot(test_plot_df, aes(x = booking_week)) +
  geom_line(aes(y = weekly_cancellations), color = "grey30") +
  geom_line(aes(y = pred), color = "#C00000", linetype = "dashed") +
  facet_wrap(~ hotel, ncol = 1, scales = "free_y") +
  labs(title = "NB model: observed (grey) vs. predicted (red) weekly cancellations",
       subtitle = "Held-out 2017 weeks",
       x = NULL, y = "Cancellations / week") +
  theme_project()
ggsave(here("report", "figures", "F07_nb_observed_vs_predicted.png"),
       p_pred, width = 8, height = 5, dpi = 200)

# Pseudo-R^2 (Cragg-Uhler)
n <- nrow(train)
ll_null  <- as.numeric(logLik(glm.nb(weekly_cancellations ~ 1 +
                                       offset(log(weekly_bookings)),
                                     data = train)))
ll_model <- as.numeric(logLik(nb_model))
pseudo_r2 <- (1 - exp((ll_null - ll_model) * 2 / n)) /
  (1 - exp(ll_null * 2 / n))
cat("Cragg-Uhler pseudo-R^2:", round(pseudo_r2, 3), "\n")

# ---- Save model + residuals (10_anomaly_residuals.R uses these) -
saveRDS(nb_model, here("data", "processed", "model_nb.rds"))

# Store residuals on the *full* dataset (train+test) so the
# anomaly script can flag weeks across the whole window.
weekly$pred <- predict(nb_model, newdata = weekly, type = "response")
weekly$deviance_resid <- residuals(nb_model,
                                   type = "deviance")[
                                     match(paste(weekly$hotel,
                                                 weekly$booking_week),
                                           paste(train$hotel,
                                                 train$booking_week))
                                   ]
# Manually compute deviance-style residuals for ALL rows (incl. test)
# using the formula: sign(y - mu) * sqrt(2 * (y*log(y/mu) - (y - mu)))
mu <- weekly$pred
y  <- weekly$weekly_cancellations
weekly$deviance_resid_all <- sign(y - mu) *
  sqrt(pmax(2 * (ifelse(y == 0, 0, y * log(y / mu)) - (y - mu)), 0))

saveRDS(weekly, here("data", "processed", "weekly_with_preds.rds"))
message("Wrote model_nb.rds and weekly_with_preds.rds")
