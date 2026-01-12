library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ------------------------------------------------------------
# 1) Paths and label definitions
# ------------------------------------------------------------
path <- "/data"
figure_path <- "/figure"

# Long descriptive labels to show on axes (wrapped for readability)
gdd_labels <- c(
  "GDD1" = "Sugar-Starch Beverage Pattern Diet",
  "GDD2" = "Dairy-Based Diet",
  "GDD3" = "Plant-Grain Pattern Diet",
  "GDD4" = "Mixed Diet Including Refined Grains",
  "GDD5" = "Protein-Rich Diet"
)

n_labels <- c(
  "N1" = "Western-European Diet Cluster",
  "N2" = "Low-Diversity Low-Income Diet Cluster",
  "N3" = "Mediterranean-East Asian Diet Cluster",
  "N4" = "Fast Food-Urban High-Sugar Diet Cluster",
  "N5" = "Meat-Centric High-Income Diet Cluster"
)

gbd_labels <- c(
  "GBD1" = "Pure Neuropsychiatry",
  "GBD2" = "Chronic Systemic and Neurodegenerative Burden",
  "GBD3" = "Infection + Maternal and Child Health + Nutrition",
  "GBD4" = "Metabolic Inflammatory Organ Invasion",
  "GBD5" = "Metabolic + Immunological Skin"
)

d_labels <- c(
  "D1" = "Infectious Diseases and Inflammation Cluster",
  "D2" = "Metabolic and Skin Disease Cluster",
  "D3" = "Average low disease burden",
  "D4" = "Noncommunicable Disease Cluster",
  "D5" = "Mental Health Burden Cluster"
)

# ------------------------------------------------------------
# 2) GDD analysis (fixed k=5)
#    - Cluster variables into GDD1–GDD5 using k-means on PCA loadings
#    - Aggregate variables within each cluster to form country-year features
#    - Cluster country-year observations into N1–N5
# ------------------------------------------------------------
GDD_Climate <- fread(file.path(path, "GDD_all_temp_data_250617.csv"))
GDD_var     <- fread(file.path(path, "GDD_var_label.csv"))
GDD_var_n   <- GDD_var[c(1:18, 47), ]$label

GDD_dat <- GDD_Climate %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(GDD_var_n)) %>%
  mutate(Coffee = Coffee * 266.8, Tea = Tea * 266.8)

# Variable clustering (GDD1–GDD5) via PCA loadings (PC1–PC3)
pca_gdd <- prcomp(dplyr::select(GDD_dat, -iso3, -year), center = TRUE, scale. = TRUE)
set.seed(123)
km_gdd_var <- kmeans(pca_gdd$rotation[, 1:3], centers = 5, nstart = 25)

# Build clustered dimensions by averaging variables within each cluster
GDD_wide <- GDD_dat %>%
  pivot_longer(-c(iso3, year), names_to = "variable") %>%
  left_join(
    tibble(variable = rownames(pca_gdd$rotation), cluster = paste0("GDD", km_gdd_var$cluster)),
    by = "variable"
  ) %>%
  group_by(iso3, year, cluster) %>%
  summarise(val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cluster, values_from = val)

# Observation clustering (N1–N5) on standardized GDD dimensions
scaled_gdd <- scale(dplyr::select(GDD_wide, starts_with("GDD")))
set.seed(123)
km_gdd_obs <- kmeans(scaled_gdd, centers = 5, nstart = 25)
GDD_wide$Ncluster <- factor(km_gdd_obs$cluster, levels = 1:5, labels = paste0("N", 1:5))

# ------------------------------------------------------------
# 3) GBD analysis (fixed k=5)
#    - Cluster variables into GBD1–GBD5 using k-means on PCA loadings
#    - Aggregate variables within each cluster to form country-year features
#    - Cluster country-year observations into D1–D5
# ------------------------------------------------------------
GBD_DALYs <- fread(file.path(path, "GBD_DALYs_rate.csv"))
GBD_var_n <- colnames(GBD_DALYs)[-c(1, 2)]

GBD_dat <- GBD_DALYs %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(GBD_var_n))

# Variable clustering (GBD1–GBD5) via PCA loadings (PC1–PC3)
pca_gbd <- prcomp(dplyr::select(GBD_dat, -iso3, -year), center = TRUE, scale. = TRUE)
set.seed(123)
km_gbd_var <- kmeans(pca_gbd$rotation[, 1:3], centers = 5, nstart = 25)

# Build clustered dimensions by averaging variables within each cluster
GBD_wide <- GBD_dat %>%
  pivot_longer(-c(iso3, year), names_to = "variable") %>%
  left_join(
    tibble(variable = rownames(pca_gbd$rotation), cluster = paste0("GBD", km_gbd_var$cluster)),
    by = "variable"
  ) %>%
  group_by(iso3, year, cluster) %>%
  summarise(val = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cluster, values_from = val)

# Observation clustering (D1–D5) on standardized GBD dimensions
scaled_gbd <- scale(dplyr::select(GBD_wide, starts_with("GBD")))
set.seed(123)
km_gbd_obs <- kmeans(scaled_gbd, centers = 5, nstart = 25)
GBD_wide$Dcluster <- factor(km_gbd_obs$cluster, levels = 1:5, labels = paste0("D", 1:5))

# ------------------------------------------------------------
# 4) Heatmap helper
#    - Supports long axis labels via line wrapping
#    - Uses a diverging scale centered at 0 (z-score mean)
# ------------------------------------------------------------
draw_heatmap <- function(data, x_col, y_col, fill_col, x_labels, y_labels, filename) {
  p <- ggplot(data, aes_string(x = x_col, y = y_col, fill = fill_col)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, limits = c(-1.5, 2), oob = scales::squish
    ) +
    # Wrap long labels for readability
    scale_x_discrete(labels = function(x) str_wrap(x_labels[x], width = 20)) +
    scale_y_discrete(labels = function(x) str_wrap(y_labels[x], width = 30)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 11),
      axis.text.y = element_text(color = "black", size = 11),
      plot.margin = margin(20, 40, 20, 20)
    )
  
  ggsave(file.path(figure_path, filename), plot = p, width = 11, height = 9, dpi = 300)
}

# ------------------------------------------------------------
# 5) Summarize cluster means and save heatmaps
# ------------------------------------------------------------

# GDD heatmap: average standardized GDD dimensions by N-cluster
gdd_mean <- as.data.frame(scaled_gdd) %>%
  mutate(Ncluster = GDD_wide$Ncluster) %>%
  group_by(Ncluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-Ncluster, names_to = "GDDdim", values_to = "mean_val")

# GBD heatmap: average standardized GBD dimensions by D-cluster
gbd_mean <- as.data.frame(scaled_gbd) %>%
  mutate(Dcluster = GBD_wide$Dcluster) %>%
  group_by(Dcluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-Dcluster, names_to = "GBDdim", values_to = "mean_val")

# Save heatmaps
draw_heatmap(gdd_mean, "GDDdim", "Ncluster", "mean_val", gdd_labels, n_labels, "GDD_final_heatmap.png")
draw_heatmap(gbd_mean, "GBDdim", "Dcluster", "mean_val", gbd_labels, d_labels, "GBD_final_heatmap.png")