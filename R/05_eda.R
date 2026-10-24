# R/05_eda.R
# ---------------------------------------------------------------
# Stage 5: Exploratory Data Analysis figures for the report.
# Generates F1 (weekly time series) and F2 (missingness) per
# blueprint §12.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(naniar)
  library(here)
})
source(here("R", "utils", "plotting.R"))

booking_level <- readRDS(here("data", "processed", "booking_level.rds"))
weekly_agg    <- readRDS(here("data", "processed", "weekly_agg.rds"))

fig_dir <- here("report", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- F1: weekly bookings vs cancellations by hotel ---------------
p1 <- weekly_agg |>
  ggplot(aes(x = booking_week)) +
  geom_line(aes(y = weekly_bookings), color = "grey50") +
  geom_line(aes(y = weekly_cancellations), color = "#C00000") +
  facet_wrap(~ hotel, ncol = 1, scales = "free_y") +
  labs(title = "Weekly bookings (grey) and cancellations (red)",
       x = NULL, y = "Count / week") +
  theme_project()

ggsave(file.path(fig_dir, "F01_weekly_bookings_cancellations.png"),
       p1, width = 8, height = 5, dpi = 200)

# ---- F2: missingness ---------------------------------------------
p2 <- booking_level |>
  dplyr::select(country, agent, company, children, adr) |>
  vis_miss(warn_large_data = FALSE) +
  labs(title = "Missingness in selected HBD columns") +
  theme_project()

ggsave(file.path(fig_dir, "F02_missingness.png"),
       p2, width = 7, height = 5, dpi = 200)

# ---- Quick distribution overview --------------------------------
p_lead <- ggplot(booking_level,
                 aes(x = lead_time, fill = factor(is_canceled))) +
  geom_histogram(binwidth = 14, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("0" = "#1F4E79", "1" = "#C00000"),
                    labels = c("Honored", "Canceled"),
                    name = NULL) +
  labs(title = "Lead time distribution by cancellation outcome",
       x = "Lead time (days)", y = "Bookings") +
  theme_project()
ggsave(file.path(fig_dir, "F0X_lead_time_dist.png"),
       p_lead, width = 7, height = 4, dpi = 200)

message("EDA figures written to ", fig_dir)
