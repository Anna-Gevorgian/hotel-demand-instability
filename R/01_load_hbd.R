# R/01_load_hbd.R
# ---------------------------------------------------------------
# Stage 1: Load the Hotel Booking Demand CSV and reconstruct
# booking_date = arrival_date - lead_time.
#
# This single line is the project's most important methodological
# move. It lets us join external data to TWO time anchors:
#   (a) booking_date  — what the customer knew when deciding
#   (b) arrival_date  — what conditions were at consumption
#
# Most prior HBD analyses use only (b) and conflate them.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(here)
})

# ---- Load ------------------------------------------------------
hbd_path <- here("data", "raw", "hotel_bookings.csv")
if (!file.exists(hbd_path)) {
  stop("Place the Kaggle hotel_bookings.csv at: ", hbd_path,
       "\nSee README > Data setup.")
}

hbd_raw <- read_csv(hbd_path, show_col_types = FALSE)
message("Loaded ", nrow(hbd_raw), " bookings.")

# ---- Parse arrival_date ----------------------------------------
# HBD encodes arrival as year + month-name + day-of-month.
# We rebuild it as a Date.
month_lookup <- setNames(1:12, month.name)

hbd <- hbd_raw |>
  mutate(
    arrival_month_num = month_lookup[arrival_date_month],
    arrival_date = make_date(
      year  = arrival_date_year,
      month = arrival_month_num,
      day   = arrival_date_day_of_month
    )
  )

# Sanity check: dates should fall in the documented range
date_range <- range(hbd$arrival_date, na.rm = TRUE)
message("Arrival dates span: ", date_range[1], " to ", date_range[2])
stopifnot(date_range[1] >= as.Date("2015-07-01"),
          date_range[2] <= as.Date("2017-09-30"))

# ---- Reconstruct booking_date ----------------------------------
# THE central methodological move.
hbd <- hbd |>
  mutate(
    booking_date = arrival_date - days(lead_time)
  )

# ---- Outcome and minimal cleaning ------------------------------
hbd <- hbd |>
  mutate(
    is_canceled = as.integer(is_canceled),
    # 'country' has ~0.4% missingness — flag rather than drop
    country = if_else(is.na(country) | country == "NULL", "UNK", country),
    # Convert booking week (Monday-anchored) for aggregate analysis later
    booking_week = floor_date(booking_date, unit = "week", week_start = 1),
    arrival_week = floor_date(arrival_date, unit = "week", week_start = 1)
  )

# ---- Quick sanity stats ----------------------------------------
cat("\n--- Sanity checks ---\n")
cat("Overall cancellation rate:",
    round(mean(hbd$is_canceled), 3), "\n")
cat("By hotel:\n")
print(hbd |>
        group_by(hotel) |>
        summarise(
          n = dplyr::n(),
          cancel_rate = mean(is_canceled),
          .groups = "drop"
        ))
cat("Country missingness rate:",
    round(mean(hbd$country == "UNK"), 4), "\n")
cat("Lead time distribution (quartiles):\n")
print(quantile(hbd$lead_time, probs = c(0, .25, .5, .75, .9, .99, 1)))

# ---- Save ------------------------------------------------------
out_path <- here("data", "processed", "hbd.rds")
saveRDS(hbd, out_path)
message("Wrote ", out_path)
