## =========================================================
## Country figures: 6 plots per country (all same size)
## 01 Disease trends (GBD1–GBD5) with slope linetype + last point icon
## 02 PHDI lines (PHDI1–PHDI3 + Total PHDI) as 4 line plot (Total = black, thicker)
## 03 Climate change from 1990 (z score change)
## 04 Economic indicators (z score)
## 05 HVI disease categories (NCD vs ID vs Other) with slope trend + last point icon
## 06 Diet categories (Helpful vs Harmful vs Context) with slope trend + last point icon
##
## Output: PNG files saved in figure_path
## All plots saved with identical size: width 8, height 2.5
## =========================================================

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(Polychrome)

## ---------------------------
## Paths
## ---------------------------
path <- "/data"
figure_path <- "/figure"
dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)

## ---------------------------
## Countries
## ---------------------------
iso3_lst <- c("ITA", "SAU", "UZB")   # change if needed

## ---------------------------
## Read data
## ---------------------------
data <- fread(file.path(path, "data_merged_quarters.csv"))

## =========================================================
## 0) Category definitions
## =========================================================


## HVI disease categories
hvi_ncd_cols <- c(
  "Cardiovascular diseases",
  "Chronic respiratory diseases",
  "Diabetes and kidney diseases",
  "Neoplasms",
  "Neurological disorders",
  "Digestive diseases",
  "Musculoskeletal disorders"
)

hvi_id_cols <- c(
  "Respiratory infections and tuberculosis",
  "Enteric infections",
  "Neglected tropical diseases and malaria",
  "Other infectious diseases",
  "Maternal and neonatal disorders",
  "Nutritional deficiencies"
)

## Diet categories
diet_helpful_cols <- c(
  "Fruits",
  "Non-starchy vegetables",
  "Beans and legumes",
  "Nuts and seeds",
  "Whole grains",
  "Total seafoods",
  "Yoghurt (including fermented milk)" ,
  "Coffee",
  "Tea"
)

diet_harmful_cols <- c(
  "Sugar-sweetened beverages",
  "Refined grains",
  "Total processed meats", 
  "Fruit juices"
)

diet_neutral_cols <- c(
  "Potatoes",
  "Other starchy vegetables",
  "Eggs",
  "Cheese",
  "Total Milk"
)

## =========================================================
## 0.5) Helpers
## =========================================================

pick1 <- function(patterns, nm) {
  hits <- unique(unlist(lapply(patterns, function(p) grep(p, nm, ignore.case = TRUE, value = TRUE))))
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

zscore_tbl <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols), ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))))
}

safe_slope <- function(y, x) {
  if (length(na.omit(y)) >= 2 && length(unique(na.omit(x))) >= 2) {
    as.numeric(coef(lm(y ~ x))[2])
  } else {
    NA_real_
  }
}

hex_to_rgb01 <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  as.numeric(rgb)
}

rel_luminance <- function(hex) {
  # WCAG relative luminance (0=black, 1=white)
  rgb <- hex_to_rgb01(hex)
  f <- function(u) ifelse(u <= 0.03928, u/12.92, ((u + 0.055)/1.055)^2.4)
  r <- f(rgb[1]); g <- f(rgb[2]); b <- f(rgb[3])
  0.2126*r + 0.7152*g + 0.0722*b
}

## =========================================================
## 1) Basic checks
## =========================================================

stopifnot(all(c("year", "iso3") %in% names(data)))

need_cols <- c(hvi_ncd_cols, hvi_id_cols, diet_helpful_cols, diet_harmful_cols, diet_neutral_cols)
missing_cols <- setdiff(need_cols, names(data))
if (length(missing_cols) > 0) {
  stop("These columns are missing in your data:\n", paste(missing_cols, collapse = ", "))
}

meta_cols <- c("year","continent","region","superregion2","iso3","Country")

## =========================================================
## 2) Disease trends (GBD1–GBD5)
## =========================================================

gbd_cols <- c("GBD1","GBD2","GBD3","GBD4","GBD5")
stopifnot(all(gbd_cols %in% names(data)))

gbd_df <- data %>%
  select(any_of(meta_cols), all_of(gbd_cols)) %>%
  filter(iso3 %in% iso3_lst) %>%
  mutate(across(all_of(gbd_cols), ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))))

gbd_long <- gbd_df %>%
  pivot_longer(cols = all_of(gbd_cols), names_to = "disease", values_to = "value")

gbd_trend_tbl <- gbd_long %>%
  group_by(iso3, disease) %>%
  summarize(slope = safe_slope(value, year), .groups = "drop") %>%
  mutate(trend = if_else(!is.na(slope) & slope >= 0, "up", "down"))

gbd_ylim <- range(gbd_long$value, na.rm = TRUE) + c(-0.2, 0.2)

gbd_pal <- c(
  GBD1 = "#e7298a",
  GBD2 = "#66a61e",
  GBD3 = "#1b9e77",
  GBD4 = "#a6761d",
  GBD5 = "#7570b3"
)

## =========================================================
## 3) Auto detect columns for climate and economics
## =========================================================

nm <- names(data)

climate_cols <- c(
  MeanT = pick1(c("^MeanT$", "mean[_\\. ]?temp", "temperature", "temp"), nm),
  PM = pick1(c("PM2\\.5", "PM25", "pm_?2\\.?5", "^PM$"), nm),
  RH = pick1(c("^RH$", "rel[_\\. ]?humidity", "humidity"), nm),
  Precipitation = pick1(c("^PR$", "^PR[_\\. ]", "precip", "precipitation", "rain", "rainfall"), nm),
  Heat_index = pick1(c("^HI$", "heat[_\\. ]?index$", "heatindex"), nm)
)
climate_cols <- climate_cols[!is.na(climate_cols)]
if (length(climate_cols) == 0) stop("No climate columns detected. Please set climate_cols manually.")

econ_cols <- c(
  Trade = pick1(c("^Trade$", "trade"), nm),
  Gdp   = pick1(c("^Gdp$", "gdp"), nm),
  Urb   = pick1(c("^Urb$", "urban|urb"), nm)
)
econ_cols <- econ_cols[!is.na(econ_cols)]
if (length(econ_cols) == 0) stop("No economic columns detected. Please set econ_cols manually.")

## Climate long with change from 1990
clim_df0 <- data %>%
  select(any_of(meta_cols), all_of(unname(climate_cols))) %>%
  filter(iso3 %in% iso3_lst) %>%
  zscore_tbl(unname(climate_cols))

clim_long <- clim_df0 %>%
  pivot_longer(cols = all_of(unname(climate_cols)),
               names_to = "metric_raw", values_to = "z") %>%
  mutate(metric = recode(metric_raw, !!!setNames(names(climate_cols), unname(climate_cols)))) %>%
  group_by(iso3, metric) %>%
  mutate(z1990 = z[year == 1990][1],
         change = z - z1990) %>%
  ungroup() %>%
  filter(!is.na(change))

clim_ylim <- range(clim_long$change, na.rm = TRUE) + c(-0.2, 0.2)

clim_col_pal <- c(
  MeanT = "#1f78b4",
  Heat_index = "#33a02c",
  PM = "#6a3d9a",
  RH = "#1b9e77",
  Precipitation = "#a6cee3"
)
clim_col_pal <- clim_col_pal[names(climate_cols)]

## Economic long z score
econ_df0 <- data %>%
  select(any_of(meta_cols), all_of(unname(econ_cols))) %>%
  filter(iso3 %in% iso3_lst) %>%
  zscore_tbl(unname(econ_cols))

econ_long <- econ_df0 %>%
  pivot_longer(cols = all_of(unname(econ_cols)),
               names_to = "metric_raw", values_to = "z") %>%
  mutate(metric = recode(metric_raw, !!!setNames(names(econ_cols), unname(econ_cols))))

econ_ylim <- range(econ_long$z, na.rm = TRUE) + c(-0.2, 0.2)

econ_col_pal <- c(
  Gdp = "#e31a1c",
  Trade = "#ff7f00",
  Urb = "#b15928"
)
econ_col_pal <- econ_col_pal[names(econ_cols)]

## =========================================================
## 4) PHDI lines (PHDI1–PHDI3 + Total PHDI)
## =========================================================

phdi_total_col <- pick1(c("^PHDI[_\\. ]*total$", "PHDI_total", "PHDIscore", "^PHDI$"), nm)

phdi_comp_cols <- grep("^Score_", nm, value = TRUE)
phdi_comp_cols <- phdi_comp_cols[!grepl("_Q$", phdi_comp_cols)]
phdi_comp_cols <- phdi_comp_cols[!grepl("total|Total|PHDI", phdi_comp_cols)]
if (length(phdi_comp_cols) == 0) stop("No PHDI component columns detected. Please check Score_ variables.")

phdi_df <- data %>%
  select(any_of(meta_cols), any_of(phdi_comp_cols), any_of(phdi_total_col)) %>%
  filter(iso3 %in% iso3_lst)

if (is.na(phdi_total_col) || !(phdi_total_col %in% names(phdi_df))) {
  phdi_df <- phdi_df %>% mutate(PHDI_total = rowSums(across(all_of(phdi_comp_cols)), na.rm = TRUE))
  phdi_total_col <- "PHDI_total"
}

gdd_phdi_map <- list(
  PHDI1 = c("Score_Whole_fruits", "Score_Nonstarchy_vegetables", "Score_Nuts_and_seeds", "Score_Legumes"),
  PHDI2 = c("Score_Whole_grains", "Score_Unsat_oils", "Score_Fish", "Score_Starchy_veg",
            "Score_Dairy", "Score_Red_meat", "Score_Eggs"),
  PHDI3 = c("Score_Sugar", "Score_Sat_fat")
)

map_cols <- unique(unlist(gdd_phdi_map))
missing_phdi_map <- setdiff(map_cols, names(phdi_df))
if (length(missing_phdi_map) > 0) {
  stop("These PHDI component columns in gdd_phdi_map are missing:\n",
       paste(missing_phdi_map, collapse = ", "))
}

for (g in names(gdd_phdi_map)) {
  cols_g <- gdd_phdi_map[[g]]
  phdi_df[[g]] <- rowSums(phdi_df[, ..cols_g], na.rm = TRUE)
}

new_phdi_vars <- names(gdd_phdi_map)

phdi_line_long <- phdi_df %>%
  mutate(PHDI_total_line = .data[[phdi_total_col]]) %>%
  select(any_of(meta_cols), all_of(new_phdi_vars), PHDI_total_line) %>%
  pivot_longer(
    cols = c(all_of(new_phdi_vars), "PHDI_total_line"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric, PHDI_total_line = "PHDI_total"),
    metric = factor(metric, levels = c(new_phdi_vars, "PHDI_total"))
  )

phdi_line_ylim <- range(phdi_line_long$value, na.rm = TRUE) + c(-0.2, 0.2)

phdi_line_pal <- c(
  PHDI1 = "#66c2a5",
  PHDI2 = "#fc8d62",
  PHDI3 = "#8da0cb",
  PHDI_total = "#000000"
)

## =========================================================
## 5) HVI categories with Other
## =========================================================

stopifnot(ncol(data) >= 43)
disease_all_cols <- names(data)[26:43]

hvi_other_cols <- setdiff(disease_all_cols, c(hvi_ncd_cols, hvi_id_cols))
if (length(hvi_other_cols) == 0) stop("hvi_other_cols is empty. Check disease_all_cols indexing.")

hvi_df <- data %>%
  select(year, iso3, Country, all_of(disease_all_cols)) %>%
  filter(iso3 %in% iso3_lst) %>%
  mutate(across(all_of(disease_all_cols),
                ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))) %>%
  mutate(
    NCD   = rowMeans(across(all_of(hvi_ncd_cols)), na.rm = TRUE),
    ID    = rowMeans(across(all_of(hvi_id_cols)), na.rm = TRUE),
    Other = rowMeans(across(all_of(hvi_other_cols)), na.rm = TRUE)
  ) %>%
  select(year, iso3, Country, NCD, ID, Other)

hvi_long <- hvi_df %>%
  pivot_longer(cols = c(NCD, ID, Other), names_to = "category", values_to = "value") %>%
  mutate(category = factor(category, levels = c("NCD", "ID", "Other")))

hvi_trend_tbl <- hvi_long %>%
  group_by(iso3, category) %>%
  summarize(slope = safe_slope(value, year), .groups = "drop") %>%
  mutate(trend = if_else(!is.na(slope) & slope >= 0, "up", "down"))

hvi_ylim <- range(hvi_long$value, na.rm = TRUE) + c(-0.2, 0.2)

## =========================================================
## 6) Diet categories
## =========================================================

diet_df <- data %>%
  select(year, iso3, Country,
         all_of(diet_helpful_cols),
         all_of(diet_harmful_cols),
         all_of(diet_neutral_cols)) %>%
  filter(iso3 %in% iso3_lst) %>%
  mutate(across(c(all_of(diet_helpful_cols),
                  all_of(diet_harmful_cols),
                  all_of(diet_neutral_cols)),
                ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))) %>%
  mutate(
    Helpful = rowMeans(across(all_of(diet_helpful_cols)), na.rm = TRUE),
    Harmful = rowMeans(across(all_of(diet_harmful_cols)), na.rm = TRUE),
    Neutral = rowMeans(across(all_of(diet_neutral_cols)), na.rm = TRUE)
  ) %>%
  select(year, iso3, Country, Helpful, Harmful, Neutral)

diet_long <- diet_df %>%
  pivot_longer(cols = c(Helpful, Harmful, Neutral),
               names_to = "category", values_to = "value") %>%
  mutate(category = factor(category, levels = c("Helpful", "Harmful", "Neutral")))

diet_trend_tbl <- diet_long %>%
  group_by(iso3, category) %>%
  summarize(slope = safe_slope(value, year), .groups = "drop") %>%
  mutate(trend = if_else(!is.na(slope) & slope >= 0, "up", "down"))

diet_ylim <- range(diet_long$value, na.rm = TRUE) + c(-0.2, 0.2)

## =========================================================
## 7) Colors: ensure HVI and Diet are different, and remove too light colors
## =========================================================

all36 <- palette36.colors(36)

# remove very light colors (hard to see). threshold can be tightened if needed
all36_ok <- all36[rel_luminance(all36) <= 0.85]

used_cols <- unique(c(unname(gbd_pal), unname(clim_col_pal), unname(econ_col_pal), unname(phdi_line_pal)))
pool <- setdiff(all36_ok, used_cols)

pick_n <- function(pool_vec, n) {
  if (length(pool_vec) < n) stop("Not enough usable colors after filtering. Lower luminance threshold or change palettes.")
  pool_vec[seq_len(n)]
}

# HVI gets first 3, Diet gets next 3 (so they cannot match)
hvi_cols <- pick_n(pool, 3)
pool2 <- setdiff(pool, hvi_cols)
diet_cols <- pick_n(pool2, 3)

## 05 HVI: softer / less saturated (pastel-ish)
hvi_pal <- c(
  NCD   = "#4E79A7",  # muted steel blue
  ID    = "#59A14F",  # muted green
  Other = "#9C755F"   # muted brown
)

## (06) Diet categories — vivid but not neon, intuitive
## 목적: 방향성 직관적 (good / bad / neutral)
diet_pal <- c(
  Helpful = "#1B9E77",  # green (beneficial)
  Harmful = "#D62728",  # red (risk)
  Neutral = "#9467BD"   # purple (neutral, non-gray)
)

## =========================================================
## 8) Theme
## =========================================================

theme_big <- theme_minimal(base_size = 16) +   # ← 여기 숫자만 키우면 됨
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
## =========================================================
## 9) Save 6 plots for each country
## =========================================================

for (cc in iso3_lst) {
  
  ## -----------------------------
  ## (01) Disease trends (GBD1–GBD5)
  ## -----------------------------
  dat_gbd <- gbd_long %>%
    filter(iso3 == cc) %>%
    left_join(gbd_trend_tbl, by = c("iso3","disease"))
  
  last_pts_gbd <- dat_gbd %>%
    group_by(disease) %>%
    slice_max(year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(icon = if_else(trend == "up", "▲", "▼"))
  
  p_gbd <- ggplot(dat_gbd, aes(x = year, y = value, color = disease, group = disease)) +
    geom_line(aes(linetype = trend), linewidth = 1.0, na.rm = TRUE) +
    geom_point(data = last_pts_gbd, size = 1.7, show.legend = FALSE) +
    geom_text(data = last_pts_gbd, aes(label = icon),
              hjust = -0.2, vjust = 0.5, size = 4, show.legend = FALSE) +
    scale_color_manual(values = gbd_pal, drop = FALSE) +
    scale_linetype_manual(values = c(up = "solid", down = "dashed"), guide = "none") +
    labs(title = paste0(cc, "  GBD1 to GBD5 trends"),
         x = NULL, y = "Standardized value (z score)", color = NULL) +
    theme_big +
    coord_cartesian(ylim = gbd_ylim, clip = "off")
  
  ggsave(file.path(figure_path, paste0("01_trend_lines_GBD_", cc, ".png")),
         p_gbd, width = 8, height = 2.5, dpi = 200)
  
  ## -----------------------------
  ## (02) PHDI lines (PHDI1–PHDI3 + Total)
  ## -----------------------------
  phdi_c <- phdi_line_long %>% filter(iso3 == cc)
  
  p_phdi <- ggplot(phdi_c, aes(x = year, y = value, color = metric, group = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.6, na.rm = TRUE) +
    scale_color_manual(values = phdi_line_pal, drop = FALSE) +
    labs(title = paste0("PHDI trends (", cc, ")"),
         x = NULL, y = "Value", color = NULL) +
    theme_big +
    coord_cartesian(ylim = phdi_line_ylim)
  
  p_phdi <- p_phdi +
    geom_line(
      data = phdi_c %>% filter(metric == "PHDI_total"),
      aes(x = year, y = value),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 1.6,
      na.rm = TRUE
    ) +
    geom_point(
      data = phdi_c %>% filter(metric == "PHDI_total"),
      aes(x = year, y = value),
      inherit.aes = FALSE,
      color = "black",
      size = 2.2,
      na.rm = TRUE
    )
  
  ggsave(file.path(figure_path, paste0("02_phdi_lines_", cc, ".png")),
         p_phdi, width = 8, height = 2.5, dpi = 200)
  
  ## -----------------------------
  ## (03) Climate change from 1990
  ## -----------------------------
  clim_c <- clim_long %>% filter(iso3 == cc)
  
  p_clim <- ggplot(clim_c, aes(x = year, y = change, color = metric, group = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.8, na.rm = TRUE) +
    scale_color_manual(values = clim_col_pal, drop = FALSE) +
    labs(title = paste0("Climate change from 1990 (", cc, ")"),
         x = NULL, y = "Change from 1990 (z score)", color = NULL) +
    theme_big +
    coord_cartesian(ylim = clim_ylim)
  
  ggsave(file.path(figure_path, paste0("03_climate_change_from_1990_", cc, ".png")),
         p_clim, width = 8, height = 2.5, dpi = 200)
  
  ## -----------------------------
  ## (04) Economic indicators (z score)
  ## -----------------------------
  econ_c <- econ_long %>% filter(iso3 == cc)
  
  p_econ <- ggplot(econ_c, aes(x = year, y = z, color = metric, group = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.6, na.rm = TRUE) +
    scale_color_manual(values = econ_col_pal, drop = FALSE) +
    labs(title = paste0("Economic indicators (", cc, ")"),
         x = NULL, y = "Z score", color = NULL) +
    theme_big +
    coord_cartesian(ylim = econ_ylim)
  
  ggsave(file.path(figure_path, paste0("04_economic_indicators_", cc, ".png")),
         p_econ, width = 8, height = 2.5, dpi = 200)
  
  ## -----------------------------
  ## (05) HVI categories (NCD vs ID vs Other) + arrows
  ## -----------------------------
  hvi_c <- hvi_long %>%
    filter(iso3 == cc) %>%
    left_join(hvi_trend_tbl, by = c("iso3","category"))
  
  last_pts_hvi <- hvi_c %>%
    group_by(category) %>%
    slice_max(year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(icon = if_else(trend == "up", "▲", "▼"))
  
  p_hvi <- ggplot(hvi_c, aes(x = year, y = value, color = category, group = category)) +
    geom_line(aes(linetype = trend), linewidth = 1.0, na.rm = TRUE) +
    geom_point(data = last_pts_hvi, size = 1.7, show.legend = FALSE) +
    geom_text(data = last_pts_hvi, aes(label = icon),
              hjust = -0.2, vjust = 0.5, size = 4, show.legend = FALSE) +
    scale_color_manual(values = hvi_pal, drop = FALSE) +
    scale_linetype_manual(values = c(up = "solid", down = "dashed"), guide = "none") +
    labs(title = paste0("HVI disease categories (", cc, ")"),
         x = NULL, y = "Z score (category mean)", color = NULL) +
    theme_big +
    coord_cartesian(ylim = hvi_ylim, clip = "off")
  
  ggsave(file.path(figure_path, paste0("05_hvi_categories_", cc, ".png")),
         p_hvi, width = 8, height = 2.5, dpi = 200)
  
  ## -----------------------------
  ## (06) Diet categories (Helpful vs Harmful vs Context) + arrows
  ## -----------------------------
  diet_c <- diet_long %>%
    filter(iso3 == cc) %>%
    left_join(diet_trend_tbl, by = c("iso3","category"))
  
  last_pts_diet <- diet_c %>%
    group_by(category) %>%
    slice_max(year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(icon = if_else(trend == "up", "▲", "▼"))
  
  p_diet <- ggplot(diet_c, aes(x = year, y = value, color = category, group = category)) +
    geom_line(aes(linetype = trend), linewidth = 1.0, na.rm = TRUE) +
    geom_point(data = last_pts_diet, size = 1.7, show.legend = FALSE) +
    geom_text(data = last_pts_diet, aes(label = icon),
              hjust = -0.2, vjust = 0.5, size = 4, show.legend = FALSE) +
    scale_color_manual(values = diet_pal, drop = FALSE) +
    scale_linetype_manual(values = c(up = "solid", down = "dashed"), guide = "none") +
    labs(title = paste0("Diet categories (", cc, ")"),
         x = NULL, y = "Z score (category mean)", color = NULL) +
    theme_big +
    coord_cartesian(ylim = diet_ylim, clip = "off")
  
  ggsave(file.path(figure_path, paste0("06_diet_categories_", cc, ".png")),
         p_diet, width = 8, height = 2.5, dpi = 200)
}