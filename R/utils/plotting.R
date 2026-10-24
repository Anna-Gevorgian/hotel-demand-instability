# R/utils/plotting.R
# Consistent visual identity across all figures.

suppressPackageStartupMessages({
  library(ggplot2)
})

#' Project-wide ggplot theme.
#' Use as: ggplot(...) + theme_project()
theme_project <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line          = element_line(color = "grey30"),
      strip.background   = element_rect(fill = "grey95", color = NA),
      strip.text         = element_text(face = "bold"),
      plot.title         = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle      = element_text(color = "grey30")
    )
}

#' Forest plot for regression coefficients with CIs.
#' Expects a data frame with columns: term, estimate, conf.low, conf.high.
#' Useful for Negative Binomial IRRs (set log_x = TRUE).
plot_forest <- function(df, log_x = FALSE,
                        title = NULL,
                        x_label = "Estimate (95% CI)") {
  df <- df[order(df$estimate), ]
  df$term <- factor(df$term, levels = df$term)
  p <- ggplot(df, aes(x = estimate, y = term)) +
    geom_vline(xintercept = if (log_x) 1 else 0,
               linetype = "dashed", color = "grey60") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, color = "grey40") +
    geom_point(size = 2.5, color = "#1F4E79") +
    labs(title = title, x = x_label, y = NULL) +
    theme_project()
  if (log_x) p <- p + scale_x_log10()
  p
}

#' Standardized coefficient bar chart for Elastic Net etc.
#' Expects: term, estimate (already standardized).
plot_coef_bars <- function(df,
                           title = NULL,
                           highlight_top = 10) {
  df <- df[order(-abs(df$estimate)), ]
  df <- head(df, highlight_top)
  df$term <- factor(df$term, levels = rev(df$term))
  ggplot(df, aes(x = estimate, y = term,
                 fill = estimate > 0)) +
    geom_col() +
    geom_vline(xintercept = 0, color = "grey30") +
    scale_fill_manual(values = c("TRUE" = "#C00000", "FALSE" = "#1F4E79"),
                      labels = c("TRUE" = "↑ cancellation",
                                 "FALSE" = "↓ cancellation"),
                      name = NULL) +
    labs(title = title, x = "Standardized coefficient", y = NULL) +
    theme_project() +
    theme(legend.position = "bottom")
}
