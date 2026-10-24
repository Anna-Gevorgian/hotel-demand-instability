# R/04_feature_engineering.R
# ---------------------------------------------------------------
# Stage 4: Engineer features defined in the blueprint §5.
#
# Behavioral:
#   - prior_cancel_rate (Laplace-smoothed)
#   - commitment_score (z-score composite)
#   - booking_complexity
#   - lead_time_bucket
#
# Calendar:
#   - booking_month_sin / booking_month_cos (cyclic)
#
# (Context features already added in 03_merge.R)
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})

booking_level <- readRDS(here("data", "processed", "booking_level.rds"))

# ---- Behavioral ------------------------------------------------
features <- booking_level |>
  mutate(
    # Laplace-smoothed prior cancellation rate per booker history
    prior_cancel_rate = previous_cancellations /
      (previous_cancellations + previous_bookings_not_canceled + 1),

    # Behavioral commitment score: 3 components, each standardized
    .car_park = required_car_parking_spaces,
    .special  = total_of_special_requests,
    .no_refund = as.integer(deposit_type == "Non Refund")
  )

# z-score each component, then sum
z <- function(x) {
  m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}
features <- features |>
  mutate(
    commitment_score = z(.car_park) + z(.special) + z(.no_refund)
  ) |>
  select(-.car_park, -.special, -.no_refund)

# Booking complexity: count-style composite, NOT z-scored
features <- features |>
  mutate(
    booking_complexity = adults + replace_na(children, 0) + babies +
      stays_in_week_nights + stays_in_weekend_nights
  )

# ---- Lead time bucket ------------------------------------------
features <- features |>
  mutate(
    lead_time_bucket = cut(
      lead_time,
      breaks  = c(-Inf, 7, 30, 90, 180, Inf),
      labels  = c("0-7", "8-30", "31-90", "91-180", "180+"),
      right   = TRUE,
      ordered_result = TRUE
    )
  )

# ---- Cyclic month encoding -------------------------------------
# Use booking_date's month, not arrival's — captures the decision context
features <- features |>
  mutate(
    booking_month     = lubridate::month(booking_date),
    booking_month_sin = sin(2 * pi * booking_month / 12),
    booking_month_cos = cos(2 * pi * booking_month / 12)
  )

# ---- Effective repeat-guest flag -------------------------------
# HBD has 'is_repeated_guest' but some rows have it set with no
# successful prior stay — likely noise. Build a stricter version.
features <- features |>
  mutate(
    is_repeated_guest_effective =
      is_repeated_guest == 1 & previous_bookings_not_canceled > 0
  )

# ---- Sanity ----------------------------------------------------
cat("\n--- Engineered feature sanity ---\n")
print(features |>
        summarise(
          n                              = dplyr::n(),
          mean_prior_cancel_rate         = round(mean(prior_cancel_rate), 4),
          mean_commitment_score          = round(mean(commitment_score), 4),
          mean_booking_complexity        = round(mean(booking_complexity), 2),
          pct_effective_repeat           = round(100 * mean(is_repeated_guest_effective), 2)
        ))

cat("\nLead time bucket distribution:\n")
print(table(features$lead_time_bucket))

# ---- Save ------------------------------------------------------
out_path <- here("data", "processed", "features.rds")
saveRDS(features, out_path)
message("Wrote ", out_path)
