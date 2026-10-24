# R/08_model_rf.R
# ---------------------------------------------------------------
# Stage 8: Random Forest cross-check for RQ1.
# Used to validate Elastic Net's top features non-parametrically
# and to generate partial dependence plots for lead_time
# (which substitutes for the dropped GAM analysis).
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ranger)
  library(pdp)
  library(ggplot2)
  library(here)
})
source(here("R", "utils", "plotting.R"))

set.seed(2026)
features <- readRDS(here("data", "processed", "features.rds"))

# Same feature set as Elastic Net Model B
rf_data <- features |>
  dplyr::select(
    is_canceled,
    prior_cancel_rate, commitment_score, booking_complexity,
    lead_time, adr,
    booking_month_sin, booking_month_cos,
    is_repeated_guest_effective,
    source_country_holiday_at_booking,
    source_country_holiday_at_arrival,
    event_active_at_booking,
    event_active_at_arrival,
    weeks_to_nearest_event_booking,
    hotel, market_segment, distribution_channel
  ) |>
  mutate(
    is_canceled = factor(is_canceled, levels = c(0, 1)),
    across(where(is.logical), as.integer),
    across(where(is.character), as.factor)
  )

train_idx <- features$booking_date < as.Date("2017-01-01")

rf <- ranger(
  is_canceled ~ .,
  data        = rf_data[train_idx, ],
  num.trees   = 500,
  mtry        = floor(sqrt(ncol(rf_data) - 1)),
  importance  = "permutation",
  probability = TRUE,
  num.threads = 4
)

cat("\n--- Random Forest OOB summary ---\n")
print(rf)

# ---- Permutation importance ------------------------------------
imp <- tibble(
  term       = names(rf$variable.importance),
  importance = as.numeric(rf$variable.importance)
) |> arrange(desc(importance))

cat("\nTop 10 features by permutation importance:\n")
print(head(imp, 10))

p_imp <- ggplot(head(imp, 12),
                aes(x = importance,
                    y = reorder(term, importance))) +
  geom_col(fill = "#1F4E79") +
  labs(title = "Random Forest: permutation importance (top 12)",
       x = "Mean decrease in OOB accuracy", y = NULL) +
  theme_project()
ggsave(here("report", "figures", "F05_rf_importance.png"),
       p_imp, width = 7, height = 5, dpi = 200)

# ---- PDP for lead_time -----------------------------------------
# substitutes for the dropped GAM analysis
pd <- partial(
  rf,
  pred.var = "lead_time",
  train    = rf_data[train_idx, ],
  prob     = TRUE,
  which.class = "1",
  grid.resolution = 30
)

p_pdp <- ggplot(pd, aes(x = lead_time, y = yhat)) +
  geom_line(color = "#C00000", linewidth = 1) +
  geom_hline(yintercept = mean(as.numeric(as.character(rf_data$is_canceled))),
             linetype = "dashed", color = "grey50") +
  labs(title = "Partial dependence: lead time → P(cancel)",
       subtitle = "Dashed line = unconditional cancellation rate",
       x = "Lead time (days)", y = "Predicted P(cancel)") +
  theme_project()
ggsave(here("report", "figures", "F06_pdp_lead_time.png"),
       p_pdp, width = 7, height = 4, dpi = 200)

# ---- Save -----------------------------------------------------
saveRDS(list(model = rf, importance = imp, pdp_lead = pd),
        here("data", "processed", "model_rf.rds"))
message("Wrote model_rf.rds")
