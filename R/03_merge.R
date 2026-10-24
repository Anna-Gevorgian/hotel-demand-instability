# R/03_merge.R
# ---------------------------------------------------------------
# Stage 3: Merge HBD with holidays and events.
#
# Key methodological move: we join holidays on BOTH the
# booking_date AND the arrival_date, producing two separate
# context features for each booking.
#
# Outputs:
#   data/processed/booking_level.rds  — one row per booking
#   data/processed/weekly_agg.rds     — one row per week × hotel
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(here)
})

hbd      <- readRDS(here("data", "processed", "hbd.rds"))
holidays <- readRDS(here("data", "processed", "holidays.rds"))
events   <- read_csv(here("data", "raw", "events.csv"),
                     show_col_types = FALSE) |>
  mutate(start_date = as.Date(start_date),
         end_date   = as.Date(end_date))

# ---- Source-country holiday flags ------------------------------
# For each booking, was the booker's country celebrating a holiday
# (a) on the booking_date?  → behavioral decision context
# (b) on the arrival_date?  → consumption-window context

build_holiday_flag <- function(bookings, holidays, anchor_col,
                               out_col) {
  # Convert holidays to a quick-lookup set keyed by country + date
  hol_key <- paste(holidays$country_code,
                   as.character(holidays$holiday_date),
                   sep = "_")
  bookings_key <- paste(bookings$country,
                        as.character(bookings[[anchor_col]]),
                        sep = "_")
  bookings[[out_col]] <- bookings_key %in% hol_key
  bookings
}

booking_level <- hbd |>
  build_holiday_flag(holidays, "booking_date",
                     "source_country_holiday_at_booking") |>
  build_holiday_flag(holidays, "arrival_date",
                     "source_country_holiday_at_arrival")

# ---- Event proximity -------------------------------------------
# For each booking, compute (signed) weeks to nearest event center
# and whether the booking_date falls inside any event window.

event_centers <- events |>
  mutate(center_date = start_date + difftime(end_date, start_date) / 2)

nearest_event_weeks <- function(d, centers) {
  if (length(d) == 0) return(integer(0))
  diffs <- outer(as.integer(d), as.integer(centers$center_date), `-`)
  # signed distance in weeks (positive = booking before event)
  signed_weeks <- diffs / 7
  # pick smallest absolute distance per row
  idx <- apply(abs(signed_weeks), 1, which.min)
  signed_weeks[cbind(seq_along(idx), idx)]
}

booking_level$weeks_to_nearest_event_booking <-
  nearest_event_weeks(booking_level$booking_date, event_centers)

booking_level$event_active_at_booking <- mapply(
  function(d) any(events$start_date <= d & events$end_date >= d),
  booking_level$booking_date
)

booking_level$event_active_at_arrival <- mapply(
  function(d) any(events$start_date <= d & events$end_date >= d),
  booking_level$arrival_date
)

# ---- Sanity checks ---------------------------------------------
cat("\n--- Booking-level merge sanity ---\n")
cat("Bookings with source-country holiday at booking:",
    round(100 * mean(booking_level$source_country_holiday_at_booking,
                     na.rm = TRUE), 2), "%\n")
cat("Bookings with source-country holiday at arrival:",
    round(100 * mean(booking_level$source_country_holiday_at_arrival,
                     na.rm = TRUE), 2), "%\n")
cat("Bookings active during a known event at booking:",
    sum(booking_level$event_active_at_booking), "\n")
cat("Bookings active during a known event at arrival:",
    sum(booking_level$event_active_at_arrival), "\n")

# ---- Save booking-level ----------------------------------------
out_path <- here("data", "processed", "booking_level.rds")
saveRDS(booking_level, out_path)
message("Wrote ", out_path)

# ---- Weekly aggregate (for the NB model) -----------------------
# Aggregate by booking_week × hotel.
# The outcome is weekly_cancellations; offset is weekly_bookings.

weekly_agg <- booking_level |>
  group_by(hotel, booking_week) |>
  summarise(
    weekly_bookings        = dplyr::n(),
    weekly_cancellations   = sum(is_canceled),
    weekly_avg_lead_time   = mean(lead_time),
    weekly_share_OTA       = mean(market_segment == "Online TA"),
    weekly_share_corporate = mean(market_segment == "Corporate"),
    weekly_share_groups    = mean(market_segment == "Groups"),
    weekly_holiday_density = mean(source_country_holiday_at_booking),
    event_active_week      = any(event_active_at_booking),
    .groups                = "drop"
  ) |>
  filter(weekly_bookings >= 5)  # drop weeks with too-thin booking volume

cat("\n--- Weekly aggregate sanity ---\n")
cat("Weeks × hotels:", nrow(weekly_agg), "\n")
cat("Cancellation count distribution:\n")
print(summary(weekly_agg$weekly_cancellations))
cat("Booking volume distribution:\n")
print(summary(weekly_agg$weekly_bookings))

# Dispersion sanity for the eventual NB model: var vs mean
ww <- weekly_agg$weekly_cancellations
cat("\nVar/Mean ratio for weekly cancellations:",
    round(var(ww) / mean(ww), 2),
    "  (>> 1 means NB is justified over Poisson)\n")

out_path_w <- here("data", "processed", "weekly_agg.rds")
saveRDS(weekly_agg, out_path_w)
message("Wrote ", out_path_w)
