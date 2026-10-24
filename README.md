# Hotel Demand Instability — Data Mining Final Project

> External shocks and internal signals: a multi-source study of cancellation risk and demand volatility in Portuguese hospitality (2015–2017).

How much of hotel cancellation risk comes from *who the customer is* versus *what was happening when they booked*? This project combines 119,390 bookings from two Portuguese hotels (2015–2017) with public holiday calendars (nager.Date API) and a hand-built events dataset to separate behavioral from contextual drivers.

**Methods**
- **Elastic Net logistic regression**: behavioral vs. behavioral + context models, compared on AUC, PR-AUC and Brier score with a **temporal train/test split** to prevent look-ahead leakage
- **Negative Binomial regression** on weekly cancellation counts (overdispersion tested against Poisson), reported as incidence rate ratios
- **Random Forest** as a non-linear cross-check of feature importance
- **k-means segmentation** of booking behavior, validated with bootstrap Jaccard stability
- **Residual-based anomaly detection** for unusual weeks
- **Shiny dashboard** (Overview · Predictions · Volatility & Anomalies) and an R Markdown report

**Stack:** R · tidyverse · glmnet · MASS · ranger · fpc · Shiny · R Markdown · httr2

---

## Prerequisites

- R ≥ 4.2
- Internet access for the holidays API (one-time fetch, then cached)
- The Hotel Booking Demand CSV (see "Data setup" below)

## First-time setup

```r
# 1. Install renv if you don't have it
install.packages("renv")

# 2. Open the project (this will trigger renv to initialize)
# In RStudio: File > Open Project > select this directory

# 3. Install all dependencies into the project library
renv::init()        # first time only — auto-detects packages from scripts
renv::snapshot()    # commit the dependency list

# 4. If you cloned this and renv.lock already exists, instead run:
renv::restore()
```

The packages used are listed in `setup.R`. If `renv` discovery misses anything, install manually:

```r
install.packages(c(
  "tidyverse", "lubridate", "httr2", "jsonlite",
  "ggplot2", "patchwork", "plotly", "naniar",
  "glmnet", "MASS", "ranger", "pdp", "fpc",
  "yardstick", "pROC",
  "shiny", "shinydashboard",
  "rmarkdown", "knitr", "kableExtra",
  "here"
))
```

## Data setup

The Hotel Booking Demand dataset is **not committed** to this repo. Download it once:

1. From Kaggle: <https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand>
2. Save as `data/raw/hotel_bookings.csv`.

Alternative source: the same dataset is in the TidyTuesday 2020-02-11 archive.

The events CSV (`data/raw/events.csv`) is provided and committed — extend it if you find more relevant 2015–2017 shocks for Portuguese tourism.

Holidays are fetched live from the `nager.Date` API on first run and cached to `data/raw/holidays/`.

## How to run, in order

Each script is independently runnable and writes its output as an `.rds` file to `data/processed/`. Re-running a later script does not require re-running earlier ones.

```r
source("R/01_load_hbd.R")             # ~10 sec
source("R/02_fetch_holidays.R")       # ~30 sec first time, instant on rerun
source("R/03_merge.R")                # ~30 sec — booking-date reconstruction happens here
source("R/04_feature_engineering.R")  # ~10 sec
source("R/05_eda.R")                  # ~1 min — generates figures
source("R/06_model_enet.R")           # ~2 min — Elastic Net (RQ1)
source("R/07_model_nb.R")             # ~30 sec — Negative Binomial (RQ2, headline)
source("R/08_model_rf.R")             # ~3 min — Random Forest cross-check
source("R/09_cluster_kmeans.R")       # ~1 min — k-means + bootstrap stability (RQ3)
source("R/10_anomaly_residuals.R")    # ~10 sec — residual anomalies (RQ4)
```

Then to render outputs:

```r
# The Shiny dashboard (live demo)
shiny::runApp("app/app.R")

# The R Markdown report
rmarkdown::render("report/report.Rmd")
```

## Folder structure

```
hotel-demand-instability/
├── README.md                        # this file
├── setup.R                          # package install script (fallback to renv)
├── R/                               # one script per analysis stage
│   ├── 01_load_hbd.R                # loads HBD, reconstructs booking_date
│   ├── 02_fetch_holidays.R          # nager.Date API client
│   ├── 03_merge.R                   # holidays + events joined to bookings
│   ├── 04_feature_engineering.R     # behavioral + context + weekly aggregates
│   ├── 05_eda.R                     # figures for report §3, §4
│   ├── 06_model_enet.R              # Elastic Net logistic — RQ1
│   ├── 07_model_nb.R                # Negative Binomial — RQ2 (headline)
│   ├── 08_model_rf.R                # Random Forest cross-check
│   ├── 09_cluster_kmeans.R          # k-means + stability — RQ3
│   ├── 10_anomaly_residuals.R       # NB-residual anomalies — RQ4
│   └── utils/
│       ├── plotting.R               # consistent ggplot theme + helpers
│       └── metrics.R                # PR-AUC, calibration helpers
├── data/
│   ├── raw/
│   │   ├── hotel_bookings.csv       # YOU DOWNLOAD THIS (see Data setup)
│   │   ├── events.csv               # hand-curated shocks 2015–2017
│   │   └── holidays/                # cached nager.Date JSON dumps
│   └── processed/                   # .rds outputs from each script
├── app/
│   └── app.R                        # single-file Shiny dashboard
└── report/
    ├── report.Rmd                   # final write-up
    └── figures/                     # auto-generated from 05_eda.R + models
```

## Reference

