## =========================================================
## PCA loadings based k=5 k-means clustering
## Outputs:
##   1) Loading biplots (PC1 PC2, PC1 PC3, PC2 PC3) without ellipses
##   2) Silhouette plot using the same cluster colors
## Notes:
##   - Both biplot and silhouette share an identical palette
##   - Ellipse overlays are intentionally removed
## =========================================================

library(data.table)
library(dplyr)

## ---------------------------
## Paths
## ---------------------------
path <- "/data"
figure_path <- "/figure"

## =========================================================
## GDD: PCA on selected dietary variables
## =========================================================

## ---------------------------
## Load data
## ---------------------------
GDD_Climate <- fread(file.path(path, "GDD_all_temp_data_250617.csv"))
GDD_var <- fread(file.path(path, "GDD_var_label.csv"))

## Select dietary variables (18 diet variables + one additional variable index 47)
GDD_var_selec <- GDD_var[c(1:18, 47), ]
GDD_var_n <- GDD_var_selec$label

## Exclude year 2020 and keep selected variables
GDD_dat_selec <- GDD_Climate %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, GDD_var_n)

## Basic NA check (should only be driven by excluded 2020 in the raw source)
any(is.na(GDD_dat_selec))
anyNA(GDD_dat_selec)

## Unit harmonization for beverage variables (if needed for consistency)
GDD_dat_selec <- GDD_dat_selec %>%
  mutate(
    Coffee = Coffee * 266.8,
    Tea = Tea * 266.8
  )

## ---------------------------
## PCA
## ---------------------------
library(ggfortify)

GDD_dat_selec_pca <- GDD_dat_selec %>% dplyr::select(-iso3, -year)
GDD_dat_selec_pca <- scale(GDD_dat_selec_pca, center = TRUE, scale = TRUE)
pca_result <- prcomp(GDD_dat_selec_pca, center = TRUE, scale. = TRUE)

## PCA summary (variance explained, etc.)
summary(pca_result)

library(ggplot2)
library(ggrepel)
library(cluster)
library(factoextra)
library(RColorBrewer)

## ---------------------------
## Loadings for PC1–PC3
## ---------------------------
load_df <- as.data.frame(pca_result$rotation[, 1:3])
colnames(load_df) <- c("PC1", "PC2", "PC3")
load_df$variable <- rownames(load_df)

## Abbreviation map for cleaner plotting labels
abbr_map <- c(
  "Fruits" = "Fruits",
  "Non-starchy vegetables" = "NSV",
  "Potatoes" = "Potato",
  "Other starchy vegetables" = "StarchVeg",
  "Beans and legumes" = "Legumes",
  "Nuts and seeds" = "NutsSeeds",
  "Whole grains" = "WholeGrain",
  "Refined grains" = "RefGrain",
  "Total processed meats" = "ProcMeat",
  "Unprocessed red meats" = "RedMeat",
  "Total seafoods" = "Seafood",
  "Eggs" = "Eggs",
  "Cheese Yoghurt (including fermented milk)" = "CheeseYog",
  "Total Milk" = "Milk",
  "Sugar-sweetened beverages" = "SSB",
  "Fruit juices" = "FruitJuice",
  "Coffee" = "Coffee",
  "Tea" = "Tea"
)
load_df$var_plot <- abbr_map[load_df$variable]

## ---------------------------
## k-means on loadings (k=5)
## ---------------------------
set.seed(123)
kmeans_result <- kmeans(load_df[, c("PC1", "PC2", "PC3")], centers = 5, nstart = 25)
load_df$cluster <- factor(kmeans_result$cluster, levels = as.character(1:5))

## Shared palette across biplot and silhouette
pal <- setNames(brewer.pal(5, "Dark2"), as.character(1:5))

## Variance explained for axis labels
var_exp <- summary(pca_result)$importance[2, 1:3] * 100
names(var_exp) <- c("PC1", "PC2", "PC3")

## ---------------------------
## Biplot helper (no ellipses)
## - Adds origin axes
## - Draws arrows from origin to loading coordinates
## - Uses ggrepel to push labels outward and reduce overlap
## ---------------------------
make_biplot <- function(xvar, yvar) {
  ## Set a tight axis range based on observed loadings
  max_val <- max(abs(load_df[[xvar]]), abs(load_df[[yvar]]))
  axis_limit <- max_val * 1.5
  
  ggplot(load_df, aes_string(x = xvar, y = yvar, color = "cluster")) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "black") +
    geom_vline(xintercept = 0, linewidth = 0.4, color = "black") +
    geom_segment(
      aes_string(x = 0, y = 0, xend = xvar, yend = yvar),
      arrow = arrow(length = unit(0.2, "cm")),
      linewidth = 0.7
    ) +
    geom_point(size = 2) +
    geom_text_repel(
      aes(label = var_plot),
      size = 5,
      show.legend = FALSE,
      max.overlaps = Inf,
      force = 60,
      force_pull = 0,
      point.padding = 0.1,
      box.padding = 0.5,
      nudge_x = 0.15 * sign(load_df[[xvar]]),
      nudge_y = 0.15 * sign(load_df[[yvar]]),
      clip = "off",
      min.segment.length = 0,
      seed = 123
    ) +
    scale_color_manual(values = pal, drop = FALSE) +
    coord_cartesian(
      xlim = c(-axis_limit, axis_limit),
      ylim = c(-axis_limit, axis_limit),
      expand = FALSE
    ) +
    labs(
      x = sprintf("%s (%.1f%%)", xvar, var_exp[xvar]),
      y = sprintf("%s (%.1f%%)", yvar, var_exp[yvar]),
      color = "Cluster"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 14, color = "black"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

p12 <- make_biplot("PC1", "PC2")
p13 <- make_biplot("PC1", "PC3")
p23 <- make_biplot("PC2", "PC3")

## Save one panel (update to p12/p13/p23 or a combined layout if desired)
ggsave(
  filename = file.path(figure_path, "GDD_PCA_kmeans_5_biplot.png"),
  plot = p12,
  width = 7, height = 6, dpi = 300
)

## ---------------------------
## Silhouette plot (same palette)
## ---------------------------
sil <- silhouette(kmeans_result$cluster, dist(load_df[, c("PC1", "PC2", "PC3")]))

p_km5_sil <- fviz_silhouette(sil) +
  scale_fill_manual(values = pal, drop = FALSE) +
  scale_color_manual(values = pal, drop = FALSE) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(figure_path, "GDD_PCA_kmeans_5_silhouette.png"),
  plot = p_km5_sil,
  width = 8, height = 4, dpi = 300
)

## =========================================================
## GBD: PCA on DALY rate categories
## =========================================================

library(patchwork)

## ---------------------------
## Load + PCA
## ---------------------------
GBD_DALYs <- fread(file.path(path, "GBD_DALYs_rate.csv"))
GBD_var_n <- colnames(GBD_DALYs)[-c(1, 2)]

GBD_DALYs_selec <- GBD_DALYs %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(GBD_var_n))

GBD_DALYs_selec_pca <- GBD_DALYs_selec %>% dplyr::select(-iso3, -year)
pca_result <- prcomp(GBD_DALYs_selec_pca, center = TRUE, scale. = TRUE)

## Variance explained for axis labels
var_exp <- summary(pca_result)$importance[2, 1:3] * 100
names(var_exp) <- c("PC1", "PC2", "PC3")

## Loadings for PC1–PC3
load_df <- as.data.frame(pca_result$rotation[, 1:3])
colnames(load_df) <- c("PC1", "PC2", "PC3")
load_df$variable <- rownames(load_df)

## Abbreviation map for plotting labels
abbr_map_gbd <- c(
  "Cardiovascular diseases" = "CVD",
  "Chronic respiratory diseases" = "ChronicResp",
  "Digestive diseases" = "Digestive",
  "Diabetes and kidney diseases" = "DiabKidney",
  "Enteric infections" = "EntericInf",
  "Maternal and neonatal disorders" = "MatNeo",
  "Mental disorders" = "Mental",
  "Musculoskeletal disorders" = "MSK",
  "Neglected tropical diseases and malaria" = "NTD_Malaria",
  "Neoplasms" = "Cancer",
  "Neurological disorders" = "Neuro",
  "Nutritional deficiencies" = "NutritionDef",
  "Other infectious diseases" = "OtherInf",
  "Other non-communicable diseases" = "OtherNCD",
  "Respiratory infections and tuberculosis" = "RespInfTB",
  "Sense organ diseases" = "SenseOrg",
  "Skin and subcutaneous diseases" = "Skin",
  "Substance use disorders" = "SubstanceUse"
)
load_df$var_plot <- abbr_map_gbd[load_df$variable]

## ---------------------------
## k-means on loadings (k=5)
## ---------------------------
set.seed(123)
kmeans_result <- kmeans(load_df[, c("PC1", "PC2", "PC3")], centers = 5, nstart = 25)
load_df$cluster <- factor(kmeans_result$cluster, levels = as.character(1:5))

## Shared palette across biplot and silhouette
pal <- setNames(brewer.pal(5, "Dark2"), as.character(1:5))

## ---------------------------
## Biplot panels (no ellipses)
## ---------------------------
make_biplot <- function(xvar, yvar) {
  max_val <- max(abs(load_df[[xvar]]), abs(load_df[[yvar]]))
  axis_limit <- max_val * 1.5
  
  ggplot(load_df, aes_string(x = xvar, y = yvar, color = "cluster")) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "black") +
    geom_vline(xintercept = 0, linewidth = 0.4, color = "black") +
    geom_segment(
      aes_string(x = 0, y = 0, xend = xvar, yend = yvar),
      arrow = arrow(length = unit(0.2, "cm")),
      linewidth = 0.7
    ) +
    geom_point(size = 2) +
    geom_text_repel(
      aes(label = var_plot),
      size = 5,
      show.legend = FALSE,
      max.overlaps = Inf,
      force = 60,
      force_pull = 0,
      point.padding = 0.1,
      box.padding = 0.5,
      nudge_x = 0.15 * sign(load_df[[xvar]]),
      nudge_y = 0.15 * sign(load_df[[yvar]]),
      clip = "off",
      min.segment.length = 0,
      seed = 123
    ) +
    scale_color_manual(values = pal, drop = FALSE) +
    coord_cartesian(
      xlim = c(-axis_limit, axis_limit),
      ylim = c(-axis_limit, axis_limit),
      expand = FALSE
    ) +
    labs(
      x = sprintf("%s (%.1f%%)", xvar, var_exp[xvar]),
      y = sprintf("%s (%.1f%%)", yvar, var_exp[yvar]),
      color = "Cluster"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 14, color = "black"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

p12 <- make_biplot("PC1", "PC2")
p13 <- make_biplot("PC1", "PC3")
p23 <- make_biplot("PC2", "PC3")

## Optional: collect the legend once (not saved below unless you switch plot = p_km5)
p_km5 <- (p12 + p13 + p23) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "right")

## Save one panel (update to p12/p13/p23 or the combined p_km5 if desired)
ggsave(
  filename = file.path(figure_path, "GBD_rate_PCA_kmeans_5_biplot.png"),
  plot = p12,
  width = 7, height = 6, dpi = 300
)

## ---------------------------
## Silhouette plot (same palette)
## ---------------------------
sil <- silhouette(kmeans_result$cluster, dist(load_df[, c("PC1", "PC2", "PC3")]))

p_km5_sil <- fviz_silhouette(sil) +
  scale_fill_manual(values = pal, drop = FALSE) +
  scale_color_manual(values = pal, drop = FALSE) +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(figure_path, "GBD_rate_PCA_kmeans_5_silhouette.png"),
  plot = p_km5_sil,
  width = 8, height = 4, dpi = 300
)