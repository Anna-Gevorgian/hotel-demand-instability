# R/02_fetch_holidays.R
# ---------------------------------------------------------------
# Stage 2: Fetch public holidays for the top source countries
# from the nager.Date API and cache locally.
#
# API: https://date.nager.at/api/v3/PublicHolidays/{year}/{code}
# - No authentication required
# - JSON response
# - Cache to disk so we only hit the API once
#
# We pick 6 countries: Portugal (PT, the destination), and the
# top 5 source countries by booking volume in HBD: GBR, PRT, FRA,
# ESP, DEU, IRL. (We'll verify the top-5 list against actual HBD
# country counts in 03_merge.R; this list covers the dominant share.)
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(here)
})

# ---- Config ----------------------------------------------------
# nager.Date uses ISO 3166-1 alpha-2 country codes
nager_codes <- c("PT", "GB", "FR", "ES", "DE", "IE")
years       <- 2015:2017
holidays_dir <- here("data", "raw", "holidays")
dir.create(holidays_dir, showWarnings = FALSE, recursive = TRUE)

# HBD uses ISO 3166-1 alpha-3 codes — we'll convert when joining
alpha2_to_alpha3 <- c(
  PT = "PRT", GB = "GBR", FR = "FRA",
  ES = "ESP", DE = "DEU", IE = "IRL"
)

# ---- Fetcher ---------------------------------------------------
fetch_one <- function(year, code) {
  cache_file <- file.path(holidays_dir, sprintf("%d_%s.json", year, code))
  if (file.exists(cache_file)) {
    message("Cache hit: ", basename(cache_file))
    return(read_json(cache_file, simplifyVector = TRUE))
  }
  url <- sprintf("https://date.nager.at/api/v3/PublicHolidays/%d/%s",
                 year, code)
  message("Fetching: ", url)
  resp <- request(url) |>
    req_user_agent("hotel-demand-instability/0.1 (academic project)") |>
    req_retry(max_tries = 3) |>
    req_perform()
  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  write_json(parsed, cache_file, auto_unbox = TRUE, pretty = TRUE)
  parsed
}

# ---- Pull everything -------------------------------------------
grid <- expand.grid(year = years, code = nager_codes,
                    stringsAsFactors = FALSE)

raw <- map2_dfr(grid$year, grid$code, function(y, c) {
  result <- fetch_one(y, c)
  # Handle empty results gracefully
  if (length(result) == 0 || is.null(result$date)) return(tibble())
  as_tibble(result) |>
    mutate(country_code_alpha2 = c, year_fetched = y)
})

# ---- Normalize -------------------------------------------------
holidays <- raw |>
  transmute(
    holiday_date = as.Date(date),
    holiday_name = localName,
    country_code = alpha2_to_alpha3[country_code_alpha2],
    is_global    = !is.na(global) & global == TRUE
  ) |>
  filter(!is.na(country_code))

cat("\n--- Holidays summary ---\n")
cat("Total holiday records:", nrow(holidays), "\n")
print(holidays |>
        group_by(country_code) |>
        summarise(n_holidays = dplyr::n(),
                  yrs = paste(range(format(holiday_date, "%Y")),
                              collapse = "–"),
                  .groups = "drop"))

# ---- Save ------------------------------------------------------
out_path <- here("data", "processed", "holidays.rds")
saveRDS(holidays, out_path)
message("Wrote ", out_path)
