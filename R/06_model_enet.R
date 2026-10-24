# R/06_model_enet.R
# ---------------------------------------------------------------
# Stage 6: Elastic Net logistic regression on individual bookings.
# Answers RQ1: behavioral vs. contextual contribution to risk.
#
# We fit TWO nested models:
#   A. behavioral_only  — just the engineered behavioral features
#   B. behavioral + context — adds source-country holiday flags
#                              and event proximity
# and compare held-out AUC + Brier.  The Δ between A and B is
# the contextual signal contribution.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(glmnet)
  library(pROC)
  library(here)
  library(ggplot2)
})
source(here("R", "utils", "metrics.R"))
source(here("R", "utils", "plotting.R"))

set.seed(2026)

features <- readRDS(here("data", "processed", "features.rds"))

# ---- Build design matrices --------------------------------------
# Numeric + ordinal + cyclic + small categorical (one-hot)
build_xy <- function(df, include_context) {
  base_cols <- c(
    "prior_cancel_rate", "commitment_score", "booking_complexity",
    "lead_time", "adr",
    "booking_month_sin", "booking_month_cos",
    "is_repeated_guest_effective"
  )
  ctx_cols <- c(
    "source_country_holiday_at_booking",
    "source_country_holiday_at_arrival",
    "event_active_at_booking",
    "event_active_at_arrival",
    "weeks_to_nearest_event_booking"
  )
  cat_cols <- c("hotel", "market_segment", "distribution_channel",
                "lead_time_bucket")
  use_cols <- if (include_context) c(base_cols, ctx_cols) else base_cols
  df_x <- df |>
    dplyr::select(all_of(use_cols), all_of(cat_cols)) |>
    mutate(across(where(is.logical), as.integer))
  x_mat <- model.matrix(~ . - 1, data = df_x)
  list(x = x_mat, y = df$is_canceled, booking_date = df$booking_date)
}

# ---- Temporal split --------------------------------------------
train_idx <- features$booking_date <  as.Date("2017-01-01")
test_idx  <- !train_idx
cat("Train bookings:", sum(train_idx),
    "  Test bookings:", sum(test_idx), "\n")

fit_and_eval <- function(include_context, label) {
  cat("\n=== Fitting model:", label, "===\n")
  full <- build_xy(features, include_context)
  x_tr <- full$x[train_idx, ]; y_tr <- full$y[train_idx]
  x_te <- full$x[test_idx,  ]; y_te <- full$y[test_idx]

  cv <- cv.glmnet(x_tr, y_tr,
                  family = "binomial",
                  alpha  = 0.5,        # ridge/lasso mix
                  nfolds = 5,
                  type.measure = "deviance")
  p_te <- as.numeric(predict(cv, newx = x_te, s = "lambda.min",
                             type = "response"))

  auc_val <- as.numeric(pROC::auc(y_te, p_te, quiet = TRUE))
  pr_val  <- pr_auc(y_te, p_te)
  brier   <- brier_score(y_te, p_te)

  cat(sprintf("Held-out  AUC: %.3f  PR-AUC: %.3f  Brier: %.4f\n",
              auc_val, pr_val, brier))

  list(cv = cv, auc = auc_val, pr = pr_val, brier = brier,
       x_test = x_te, y_test = y_te, p_test = p_te)
}

mod_A <- fit_and_eval(include_context = FALSE, "Behavioral only")
mod_B <- fit_and_eval(include_context = TRUE,  "Behavioral + context")

# ---- Variance decomposition snapshot ---------------------------
delta_auc <- mod_B$auc   - mod_A$auc
delta_pr  <- mod_B$pr    - mod_A$pr
delta_br  <- mod_A$brier - mod_B$brier  # lower is better
cat("\n--- Behavioral → Behavioral + context delta ---\n")
cat(sprintf("Δ AUC   : %+.4f\n", delta_auc))
cat(sprintf("Δ PR-AUC: %+.4f\n", delta_pr))
cat(sprintf("Δ Brier : %+.4f  (positive = context helps)\n", delta_br))

# ---- Coefficient bar chart (Model B) ---------------------------
coefs_B <- as.matrix(coef(mod_B$cv, s = "lambda.min"))
coefs_df <- data.frame(
  term     = rownames(coefs_B),
  estimate = coefs_B[, 1]
) |>
  filter(term != "(Intercept)", estimate != 0)

p_coef <- plot_coef_bars(
  coefs_df,
  title = "Elastic Net: non-zero standardized coefficients (top 10 |β|)"
)
ggsave(here("report", "figures", "F04_enet_coefs.png"),
       p_coef, width = 7, height = 5, dpi = 200)

# ---- Save Model B for the Shiny app ----------------------------
saveRDS(list(model = mod_B$cv,
             coefs = coefs_df,
             auc   = mod_B$auc,
             pr    = mod_B$pr,
             brier = mod_B$brier),
        here("data", "processed", "model_enet.rds"))
message("Wrote model_enet.rds")
