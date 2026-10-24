# setup.R
# Fallback package installation if renv is not used.
# Run this once after cloning the repo.

required_packages <- c(
  # Data manipulation
  "tidyverse", "lubridate", "here",
  # External data fetching
  "httr2", "jsonlite",
  # Visualization
  "ggplot2", "patchwork", "plotly", "naniar",
  # Modeling
  "glmnet",      # Elastic Net
  "MASS",        # glm.nb for Negative Binomial
  "ranger",      # fast Random Forest
  "pdp",         # partial dependence plots
  "fpc",         # bootstrap cluster stability
  # Evaluation
  "yardstick", "pROC",
  # Shiny
  "shiny", "shinydashboard",
  # Reporting
  "rmarkdown", "knitr", "kableExtra"
)

missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All required packages already installed.")
}

# Optional: capture session info for reproducibility appendix
writeLines(capture.output(sessionInfo()), "session_info_setup.txt")
