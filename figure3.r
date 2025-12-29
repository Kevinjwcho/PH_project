## =========================================================
## Figure 3
## Climate PCA and k-means clustering (k = 5)
##
## Outputs:
##   - PCA scatter plot with cluster labels
##   - Silhouette plot
##   - PC1 density by cluster
##   - PC2 boxplot by cluster
##
## Notes:
##   - PCA is performed on standardized climate variables
##   - Clustering is applied to the first two PCs
##   - Random seed is fixed for reproducibility
## =========================================================

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
library(factoextra)

# ------------------------------------------------------------
# Paths (assumes project root)
# ------------------------------------------------------------
data_path   <- "data"
figure_path <- "figures/figure3"

if (!dir.exists(figure_path)) {
  dir.create(figure_path, recursive = TRUE)
}

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
# Expected input:
# GDD_all_temp_data_25y.csv
# Columns: PM25, Temperature, HeatIndex, Humidity, Precipitation
GDD_Climate <- fread(file.path(data_path, "GDD_all_temp_data_25y.csv"))

# ------------------------------------------------------------
# Select variables for PCA
# ------------------------------------------------------------
pca_vars <- GDD_Climate %>%
  select(
    PM25,
    Temperature,
    HeatIndex,
    Humidity,
    Precipitation
  )

# ------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------
pca_res <- prcomp(pca_vars, scale. = TRUE)

# ------------------------------------------------------------
# K-means clustering on first two PCs
# ------------------------------------------------------------
set.seed(123)
k <- 5

km_res <- kmeans(
  pca_res$x[, 1:2],
  centers = k,
  nstart = 50
)

plot_df <- data.frame(
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2],
  cluster = factor(km_res$cluster)
)

# ------------------------------------------------------------
# Figure 3A: PCA scatter plot
# ------------------------------------------------------------
p_pca <- ggplot(plot_df, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "PCA-based Climate Clustering (k = 5)",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  theme_bw()

ggsave(
  file.path(figure_path, "figure3A_pca_scatter.png"),
  p_pca,
  width = 6,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 3B: Silhouette plot
# ------------------------------------------------------------
sil <- silhouette(km_res$cluster, dist(pca_res$x[, 1:2]))

p_sil <- fviz_silhouette(sil) +
  labs(title = "Silhouette Plot (k = 5)") +
  theme_bw()

ggsave(
  file.path(figure_path, "figure3B_silhouette.png"),
  p_sil,
  width = 6,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 3C: PC1 density by cluster
# ------------------------------------------------------------
p_density <- ggplot(plot_df, aes(x = PC1, fill = cluster)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "PC1 Distribution by Cluster",
    x = "PC1",
    y = "Density",
    fill = "Cluster"
  ) +
  theme_bw()

ggsave(
  file.path(figure_path, "figure3C_pc1_density.png"),
  p_density,
  width = 6,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure 3D: PC2 boxplot by cluster
# ------------------------------------------------------------
p_box <- ggplot(plot_df, aes(x = cluster, y = PC2, fill = cluster)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  labs(
    title = "PC2 Distribution by Cluster",
    x = "Cluster",
    y = "PC2"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(
  file.path(figure_path, "figure3D_pc2_boxplot.png"),
  p_box,
  width = 6,
  height = 5,
  dpi = 300
)