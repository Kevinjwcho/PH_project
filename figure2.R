library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(writexl)
library(stringr)


path = "/Users/jinwoocho/Library/CloudStorage/Dropbox/Projects/PH_project/data"
figure_path = "/Users/jinwoocho/Library/CloudStorage/Dropbox/Projects/PH_project/figures2"

# read in data

data = fread(file.path(path, "data_merged_quarters.csv"))

sub_data = data %>% dplyr::select(Climate_cluster_5, GDD1, GDD2, GDD3, GDD4, GDD5,
                                  GBD1, GBD2, GBD3, GBD4, GBD5
                                  )

climate1_data = sub_data %>% filter(Climate_cluster_5 == 1)
climate2_data = sub_data %>% filter(Climate_cluster_5 == 2)
climate3_data = sub_data %>% filter(Climate_cluster_5 == 3)
climate4_data = sub_data %>% filter(Climate_cluster_5 == 4)
climate5_data = sub_data %>% filter(Climate_cluster_5 == 5)

gdd_vars <- paste0("GDD", 1:5)
gbd_vars <- paste0("GBD", 1:5)


library(tidyr)
library(ggplot2)

gdd_vars <- paste0("GDD", 1:5)
gbd_vars <- paste0("GBD", 1:5)

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  if (sd(x) == 0 || sd(y) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, method = method))
}

make_cluster_cor_long <- function(df, cluster_id, method = "pearson") {
  dfc <- df %>% filter(Climate_cluster_5 == cluster_id)
  
  cor_mat <- outer(
    gdd_vars, gbd_vars,
    Vectorize(function(gdd, gbd) safe_cor(dfc[[gdd]], dfc[[gbd]], method = method))
  )
  dimnames(cor_mat) <- list(GDD = gdd_vars, GBD = gbd_vars)
  
  as.data.frame(cor_mat) %>%
    tibble::rownames_to_column("GDD") %>%
    pivot_longer(-GDD, names_to = "GBD", values_to = "cor") %>%
    mutate(Climate_cluster_5 = cluster_id)
}

library(dplyr)
library(tidyr)
library(ggplot2)

gdd_vars <- paste0("GDD", 1:5)
gbd_vars <- paste0("GBD", 1:5)

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  if (sd(x) == 0 || sd(y) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, method = method))
}

make_cluster_cor_long <- function(df, cluster_id, method = "pearson") {
  dfc <- df %>% filter(Climate_cluster_5 == cluster_id)
  
  cor_mat <- outer(
    gdd_vars, gbd_vars,
    Vectorize(function(gdd, gbd) safe_cor(dfc[[gdd]], dfc[[gbd]], method = method))
  )
  dimnames(cor_mat) <- list(GDD = gdd_vars, GBD = gbd_vars)
  
  as.data.frame(cor_mat) %>%
    tibble::rownames_to_column("GDD") %>%
    pivot_longer(-GDD, names_to = "GBD", values_to = "cor") %>%
    mutate(Climate_cluster_5 = cluster_id)
}

# plot_cluster_heatmap <- function(long_df, cluster_id) {
#   ggplot(
#     filter(long_df, Climate_cluster_5 == cluster_id) %>%
#       mutate(GDD = factor(GDD, levels = rev(paste0("GDD", 1:5)))),
#     aes(x = GBD, y = GDD, fill = cor)
#   ) +
#     geom_tile() +
#     scale_fill_gradient2(
#       low = "#2166AC",   # blue = negative
#       mid = "white",
#       high = "#B2182B",  # red = positive
#       midpoint = 0,
#       limits = c(-1, 1),
#       na.value = "grey90"
#     ) +
#     coord_equal() +
#     labs(
#       x = "GBD",
#       y = "GDD",
#       fill = "Correlation"
#     ) +
#     theme_minimal(base_size = 16) +
#     theme(
#       panel.grid = element_blank(),
#       plot.title = element_blank()
#     )
# }

plot_cluster_heatmap <- function(long_df, cluster_id) {
  
  # GDD 및 GBD 전체 이름 정의
  gdd_labels <- c(
    "GDD1" = "Sugar-Starch Beverage Pattern Diet",
    "GDD2" = "Dairy-Based Diet",
    "GDD3" = "Plant-Grain Pattern Diet",
    "GDD4" = "Mixed Diet Including Refined Grains",
    "GDD5" = "Protein-Rich Diet"
  )
  
  gbd_labels <- c(
    "GBD1" = "Pure Neuropsychiatry",
    "GBD2" = "Chronic Systemic and Neurodegenerative",
    "GBD3" = "Infection + Maternal/Child/Nutrition", # 너무 길어 약간 축약
    "GBD4" = "Metabolic Inflammatory Organ Invasion",
    "GBD5" = "Metabolic + Immunological Skin"
  )
  
  ggplot(
    filter(long_df, Climate_cluster_5 == cluster_id) %>%
      mutate(GDD = factor(GDD, levels = rev(paste0("GDD", 1:5)))),
    aes(x = GBD, y = GDD, fill = cor)
  ) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), na.value = "grey90"
    ) +
    # ID 대신 정의된 라벨로 교체
    scale_x_discrete(labels = gbd_labels) +
    scale_y_discrete(labels = gdd_labels) + 
    coord_equal() +
    labs(x = "Disease Outcomes (GBD)", y = "Dietary Patterns (GDD)", fill = "Correlation") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      # X축 레이블이 길므로 45도 회전
      axis.text.x = element_text(size = 13, angle = 45, vjust = 1, hjust = 1, color = "black"), # X축 글자 크기
      axis.text.y = element_text(size = 13, color = "black"),                                 # Y축 글자 크기
      axis.title = element_text(size = 16, face = "bold"),                                   # 축 제목 크기
      legend.title = element_text(size = 14),                                                # 범례 제목 크기
      legend.text = element_text(size = 12)
    )
}
# ---- run for all clusters in your data ----
clusters <- sort(unique(sub_data$Climate_cluster_5))

cor_long_all <- bind_rows(lapply(clusters, \(k) make_cluster_cor_long(sub_data, k)))

plots <- lapply(clusters, \(k) plot_cluster_heatmap(cor_long_all, k))

# save (optional)
dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)
for (i in seq_along(clusters)) {
  k <- clusters[i]
  ggsave(
    filename = file.path(figure_path, paste0("heatmap_GDD_GBD_climate", k, ".png")),
    plot = plots[[i]],
    width = 8, height = 6.5, dpi = 300
  )
}
