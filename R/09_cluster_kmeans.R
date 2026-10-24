# R/09_cluster_kmeans.R
# ---------------------------------------------------------------
# Stage 9: k-means clustering on standardized numeric behavioral
# features.  Answers RQ3: behavioral segmentation.
#
# Stability checked via fpc::clusterboot (Jaccard ≥ 0.6 per
# cluster is the conventional threshold for "real").
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(fpc)
  library(ggplot2)
  library(here)
  library(tidyr)
})
source(here("R", "utils", "plotting.R"))

set.seed(2026)
features <- readRDS(here("data", "processed", "features.rds"))

# ---- Feature matrix for clustering -----------------------------
# Standardize numeric behavioral features only — keeps interpretation clean
clust_cols <- c("prior_cancel_rate", "commitment_score",
                "booking_complexity", "lead_time", "adr")

X <- features |>
  dplyr::select(all_of(clust_cols)) |>
  mutate(across(everything(), function(x) {
    if (sd(x, na.rm = TRUE) == 0) return(x)
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  })) |>
  as.matrix()

# ---- Elbow + silhouette over k -----------------------------------
# Subsample for speed; the cluster structure is stable for ~10-20k rows
samp_idx <- sample(seq_len(nrow(X)), size = min(20000, nrow(X)))
X_samp <- X[samp_idx, ]

ks <- 2:6
wcss <- sapply(ks, function(k) {
  km <- kmeans(X_samp, centers = k, nstart = 10, iter.max = 30)
  km$tot.withinss
})

elbow_df <- data.frame(k = ks, wcss = wcss)
cat("\n--- Elbow ---\n")
print(elbow_df)
p_elbow <- ggplot(elbow_df, aes(x = k, y = wcss)) +
  geom_line() + geom_point(size = 3, color = "#1F4E79") +
  labs(title = "k-means: within-cluster SS by k",
       x = "k", y = "WCSS") +
  theme_project()
ggsave(here("report", "figures", "F09a_elbow.png"),
       p_elbow, width = 6, height = 4, dpi = 200)

# Pick k by eyeballing elbow — default to 4; user may override
k_final <- 4

# ---- Bootstrap stability via clusterboot -----------------------
cat("\nRunning clusterboot for k =", k_final, "(may take ~1-2 min)...\n")
cb <- clusterboot(
  X_samp,
  B            = 50,
  clustermethod = kmeansCBI,
  k            = k_final,
  seed         = 2026,
  count        = FALSE
)

cat("\n--- Bootstrap Jaccard means per cluster ---\n")
print(round(cb$bootmean, 3))
cat("Clusters with Jaccard >= 0.6 are considered stable.\n")

# ---- Refit on full data with chosen k --------------------------
km_final <- kmeans(X, centers = k_final, nstart = 20, iter.max = 50)

features$cluster <- factor(km_final$cluster)

# ---- Profile clusters ------------------------------------------
profile <- features |>
  group_by(cluster) |>
  summarise(
    n                       = dplyr::n(),
    pct_total               = round(100 * dplyr::n() / nrow(features), 1),
    mean_lead_time          = round(mean(lead_time), 1),
    mean_adr                = round(mean(adr), 1),
    mean_prior_cancel_rate  = round(mean(prior_cancel_rate), 3),
    mean_commitment         = round(mean(commitment_score), 2),
    mean_booking_complexity = round(mean(booking_complexity), 1),
    cancel_rate             = round(mean(is_canceled), 3),
    .groups = "drop"
  )

cat("\n--- Cluster profile ---\n")
print(profile)

# Cluster x market segment cross-tab (recovers the categorical info)
cat("\n--- Cluster × market_segment ---\n")
print(table(features$cluster, features$market_segment))

# ---- Save ------------------------------------------------------
saveRDS(list(
  model      = km_final,
  profile    = profile,
  stability  = cb$bootmean,
  k          = k_final,
  cluster_assignments = features$cluster
), here("data", "processed", "model_kmeans.rds"))

# Add cluster assignment to the features table for downstream use
saveRDS(features, here("data", "processed", "features_with_clusters.rds"))

message("Wrote model_kmeans.rds and features_with_clusters.rds")
