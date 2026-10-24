# R/utils/metrics.R
# Evaluation metrics that are not in base R.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Brier score for binary outcomes.
#' @param y_true factor or 0/1 vector of true class
#' @param p_pred numeric vector of predicted probabilities for class "1" (or TRUE)
brier_score <- function(y_true, p_pred) {
  y <- as.numeric(as.character(y_true))
  if (any(is.na(y))) stop("y_true must be coercible to numeric 0/1")
  mean((p_pred - y)^2)
}

#' Calibration table: bin predicted probabilities and report
#' mean predicted vs. observed positive rate per bin.
calibration_table <- function(y_true, p_pred, n_bins = 10) {
  y <- as.numeric(as.character(y_true))
  bins <- cut(p_pred, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)
  data.frame(prob_bin = bins, predicted = p_pred, observed = y) |>
    group_by(prob_bin) |>
    summarise(
      n              = dplyr::n(),
      mean_predicted = mean(predicted, na.rm = TRUE),
      mean_observed  = mean(observed,  na.rm = TRUE),
      .groups        = "drop"
    )
}

#' Precision-Recall AUC by trapezoidal integration.
#' Returns scalar between 0 and 1.
pr_auc <- function(y_true, p_pred) {
  y  <- as.numeric(as.character(y_true))
  ord <- order(p_pred, decreasing = TRUE)
  y  <- y[ord]
  tp <- cumsum(y)
  fp <- cumsum(1 - y)
  recall    <- tp / sum(y)
  precision <- tp / (tp + fp)
  # Prepend (recall=0, precision=1) for proper AUC calculation
  recall    <- c(0, recall)
  precision <- c(1, precision)
  sum(diff(recall) * (head(precision, -1) + tail(precision, -1)) / 2)
}
