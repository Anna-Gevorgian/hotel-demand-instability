# R/10_anomaly_residuals.R
# ---------------------------------------------------------------
# Stage 10: Anomaly detection via Negative Binomial residuals.
# Answers RQ4: which weeks are anomalous, and do they align
# with the curated event timeline?
#
# Rationale: a week is "anomalous" not when its features are
# unusual (Isolation Forest's definition), but when the OUTCOME
# is unusual GIVEN the features. That's exactly what model
# residuals measure.  We reuse the NB model from script 07
# rather than fitting a new anomaly-specific model.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(here)
})
source(here("R", "utils", "plotting.R"))

weekly_preds <- readRDS(here("data", "processed", "weekly_with_preds.rds"))
events       <- read_csv(here("data", "raw", "events.csv"),
                         show_col_types = FALSE) |>
  mutate(start_date = as.Date(start_date),
         end_date   = as.Date(end_date))

# ---- Flag anomalous weeks --------------------------------------
threshold <- 2  # standard deviance-residual cutoff
weekly_preds <- weekly_preds |>
  mutate(
    is_anomaly = abs(deviance_resid_all) > threshold,
    direction  = case_when(
      deviance_resid_all >  threshold ~ "Above expected",
      deviance_resid_all < -threshold ~ "Below expected",
      TRUE                            ~ "Within range"
    )
  )

n_anom <- sum(weekly_preds$is_anomaly)
cat("\n--- Anomaly detection summary ---\n")
cat("Total weeks×hotels      :", nrow(weekly_preds), "\n")
cat("Flagged anomalies (|r|>2):", n_anom,
    sprintf("(%.1f%%)\n", 100 * n_anom / nrow(weekly_preds)))

# ---- Align with curated event timeline -------------------------
# A week is "event-aligned" if any event overlaps that 7-day window
weekly_preds$week_end <- weekly_preds$booking_week + 6
weekly_preds$event_aligned <- mapply(function(s, e) {
  any(events$start_date <= e & events$end_date >= s)
}, weekly_preds$booking_week, weekly_preds$week_end)

# Precision: fraction of anomalous weeks that overlap a known event
anom_rows <- weekly_preds |> filter(is_anomaly)
precision <- mean(anom_rows$event_aligned)
cat("Precision vs. curated events:", round(precision, 3),
    "  (Note: many real shocks are unlabeled; low precision is informative, not a failure.)\n")

# ---- Summary table of anomalies ---------------------------------
anom_summary <- anom_rows |>
  arrange(desc(abs(deviance_resid_all))) |>
  dplyr::select(hotel, booking_week, weekly_bookings,
                weekly_cancellations, pred,
                deviance_resid_all, direction, event_aligned) |>
  mutate(across(c(pred, deviance_resid_all), \(x) round(x, 2)))

cat("\n--- Top 15 anomalous weeks ---\n")
print(head(anom_summary, 15))

# ---- Figure F11 -------------------------------------------------
event_lines <- events |>
  mutate(mid = start_date + difftime(end_date, start_date) / 2) |>
  dplyr::select(event_id, mid)

p_anom <- ggplot(weekly_preds,
                 aes(x = booking_week, y = deviance_resid_all)) +
  geom_hline(yintercept = c(-threshold, threshold),
             linetype = "dashed", color = "grey50") +
  geom_point(aes(color = is_anomaly), alpha = 0.7) +
  geom_vline(data = event_lines, aes(xintercept = mid),
             linetype = "dotted", color = "#C00000") +
  scale_color_manual(values = c("TRUE" = "#C00000", "FALSE" = "grey60"),
                     name = NULL,
                     labels = c("TRUE" = "Anomaly", "FALSE" = "Within range")) +
  facet_wrap(~ hotel, ncol = 1) +
  labs(title = "NB deviance residuals by week",
       subtitle = "Dotted red verticals = curated events",
       x = NULL, y = "Deviance residual") +
  theme_project() +
  theme(legend.position = "bottom")

ggsave(here("report", "figures", "F11_anomalies.png"),
       p_anom, width = 9, height = 5, dpi = 200)

# ---- Save -----------------------------------------------------
saveRDS(list(
  weekly_with_anomalies = weekly_preds,
  anomaly_summary       = anom_summary,
  precision_vs_events   = precision,
  threshold             = threshold
), here("data", "processed", "anomalies.rds"))
message("Wrote anomalies.rds")
