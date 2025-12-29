############################################################
## Full preprocessing, clustering, and merging pipeline
##
## This script combines the following original scripts:
## 1) Climate_clustering_fin.R
## 2) GDD_v2_clustering_fin.R
## 3) GBD_clustering_fin.R
## 4) join_clusted_data.R
## 5) Final data merging with World Bank indicators
##
## Purpose:
## - Reproduce all clustering steps (K = 5)
## - Construct the final merged analysis dataset
##
## Designed for GitHub documentation and reproducibility.
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

data_path <- "data"

## =========================================================
## 1. Climate clustering
## =========================================================

climate_raw <- fread(
  file.path(data_path, "GDD_all_temp_data_250617.csv")
)

climate_vars <- c("PM25", "MeanT", "HEAT_index", "RH", "PR")

climate_dat <- climate_raw %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(climate_vars)) %>%
  group_by(iso3, year) %>%
  summarise(across(everything(), mean, na.rm = TRUE),
            .groups = "drop")

climate_scaled <- climate_dat %>%
  mutate(across(all_of(climate_vars), scale))

set.seed(123)
climate_km <- kmeans(
  climate_scaled[, climate_vars],
  centers = 5,
  nstart = 50
)

climate_cluster <- climate_dat %>%
  mutate(Climate_cluster_5 = climate_km$cluster)

## =========================================================
## 2. Dietary (GDD) clustering
## =========================================================

GDD_var <- fread(
  file.path(data_path, "GDD_var_label.csv")
)

GDD_var_selec <- GDD_var[c(1:18, 47), "label"]

GDD_raw <- fread(
  file.path(data_path, "GDD_all_temp_data_250617.csv")
)

GDD_dat <- GDD_raw %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(GDD_var_selec)) %>%
  group_by(iso3, year) %>%
  summarise(across(everything(), mean, na.rm = TRUE),
            .groups = "drop")

GDD_scaled <- GDD_dat %>%
  mutate(across(all_of(GDD_var_selec), scale))

set.seed(123)
GDD_km <- kmeans(
  GDD_scaled[, GDD_var_selec],
  centers = 5,
  nstart = 50
)

GDD_cluster <- GDD_dat %>%
  mutate(GDD_cluster_5 = GDD_km$cluster)

## =========================================================
## 3. Disease burden (GBD) clustering
## =========================================================

GBD_raw <- fread(
  file.path(data_path, "GBD_DALYs_rate.csv")
)

GBD_vars <- grep("^GBD", names(GBD_raw), value = TRUE)

GBD_dat <- GBD_raw %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(GBD_vars)) %>%
  group_by(iso3, year) %>%
  summarise(across(everything(), mean, na.rm = TRUE),
            .groups = "drop")

GBD_scaled <- GBD_dat %>%
  mutate(across(all_of(GBD_vars), scale))

set.seed(123)
GBD_km <- kmeans(
  GBD_scaled[, GBD_vars],
  centers = 5,
  nstart = 50
)

GBD_cluster <- GBD_dat %>%
  mutate(GBD_cluster_5 = GBD_km$cluster)

## =========================================================
## 4. Join clustered data
## =========================================================

cluster_all <- climate_cluster %>%
  dplyr::select(iso3, year, Climate_cluster_5) %>%
  left_join(
    GDD_cluster %>% dplyr::select(iso3, year, GDD_cluster_5),
    by = c("iso3", "year")
  ) %>%
  left_join(
    GBD_cluster %>% dplyr::select(iso3, year, GBD_cluster_5),
    by = c("iso3", "year")
  )

## =========================================================
## 5. Merge clusters with full GDD + GBD data
## =========================================================

GDD_full <- climate_raw %>%
  filter(year != 2020) %>%
  dplyr::select(iso3, year, all_of(GDD_var_selec))

final_data <- merge(
  GDD_full,
  GBD_raw,
  by = c("iso3", "year"),
  all.x = TRUE
)

final_data <- merge(
  final_data,
  cluster_all,
  by = c("iso3", "year"),
  all.x = TRUE
)

## =========================================================
## 6. Merge World Bank indicators
## =========================================================

WB_list <- list.files(
  path = file.path(data_path, "WB"),
  full.names = TRUE
)

WB_data <- lapply(WB_list, fread)

WB_data_sel <- lapply(
  WB_data,
  function(df) df %>%
    dplyr::select(REF_AREA, TIME_PERIOD, OBS_VALUE)
)

ref_tab <- final_data %>%
  dplyr::select(iso3, year)

var_n <- c("trade", "gdp", "urb")

for (i in seq_along(WB_data_sel)) {
  
  colnames(WB_data_sel[[i]]) <- c("iso3", "year", var_n[i])
  
  ref_tab <- merge(
    ref_tab,
    WB_data_sel[[i]],
    by = c("iso3", "year"),
    all.x = TRUE
  )
}

final_data <- merge(
  final_data,
  ref_tab,
  by = c("iso3", "year"),
  all.x = TRUE
)

## =========================================================
## 7. Final checks
## =========================================================

colSums(is.na(final_data))

cor(log(final_data$gdp), final_data$urb, use = "complete.obs")
cor(log(final_data$gdp), final_data$trade, use = "complete.obs")
cor(final_data$urb, final_data$trade, use = "complete.obs")

## =========================================================
## Optional: save final dataset
## =========================================================
# fwrite(
#   final_data,
#   file.path(data_path, "final_merged_dataset.csv")
# )

## ======================= END =============================