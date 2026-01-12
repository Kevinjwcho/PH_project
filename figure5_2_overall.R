library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(patchwork)
library(scales)

sf::sf_use_s2(FALSE)

years_pair <- c(1990, 2018)

## ---------------------------------------------------------
## 0) Parameters for automatic zoom-box sizing
##    - base_dx, base_dy: base half width/height (degrees)
##    - max_dx,  max_dy : upper bounds to avoid oversized boxes for very large countries
##    - top_n_countries : cap the number of zoom panels (suggest ~30–60 for slides)
## ---------------------------------------------------------
base_dx <- 6
base_dy <- 4.5
max_dx  <- 18
max_dy  <- 14

top_n_countries <- 50
ncol_zoom <- 5

## ---------------------------
## Colors (Climate clusters)
## ---------------------------
cluster_cols <- c(
  "C1" = "#1f78b4",
  "C2" = "#33a02c",
  "C3" = "#fb9a99",
  "C4" = "#e31a1c",
  "C5" = "#6a3d9a"
)

## ---------------------------
## Read world map
## ---------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(4326) %>%
  mutate(iso3 = iso_a3)

## ---------------------------
## Read data
## ---------------------------
library(data.table)

path <- "/data"
figure_path <- "/figure"

map_df <- fread(file.path(path, "data_merged_quarters.csv")) %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(year %in% years_pair) %>%
  distinct(iso3, year, Climate_cluster_5) %>%
  mutate(cluster = paste0("C", Climate_cluster_5)) %>%
  left_join(world %>% select(iso3, geometry), by = "iso3") %>%
  st_as_sf()

## ---------------------------------------------------------
## 1) Identify countries whose cluster changes between 1990 and 2018 (iso3)
## ---------------------------------------------------------
df_pair <- map_df %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(year %in% years_pair) %>%
  st_as_sf()

chg_tbl <- df_pair %>%
  st_drop_geometry() %>%
  distinct(iso3, year, cluster) %>%
  mutate(cluster = as.character(cluster)) %>%
  pivot_wider(names_from = year, values_from = cluster, names_prefix = "y") %>%
  mutate(
    changed = !is.na(y1990) & !is.na(y2018) & (y1990 != y2018),
    change_label = if_else(changed, paste0(y1990, " \u2192 ", y2018), NA_character_)
  ) %>%
  filter(changed)

chg_only <- df_pair %>%
  filter(iso3 %in% chg_tbl$iso3) %>%
  distinct(iso3, geometry) %>%
  left_join(chg_tbl %>% select(iso3, change_label), by = "iso3")

## ---------------------------------------------------------
## 2) Representative point (point-on-surface) + area-based automatic box sizing
##    - area is computed in an equal-area CRS and mapped smoothly via log scaling
## ---------------------------------------------------------

# Representative point: guarantees the point lies inside the polygon (instead of using the centroid)
rep_pt <- st_point_on_surface(chg_only)

# Area computation (equal-area; EPSG:6933 recommended)
area_m2 <- chg_only %>%
  st_transform(6933) %>%
  st_area() %>%
  as.numeric()

# Rescale area to 0–1 (log-smoothed)
area_score <- rescale(log1p(area_m2), to = c(0, 1))

# Representative point coordinates
xy <- st_coordinates(rep_pt) %>%
  as.data.frame() %>%
  transmute(
    iso3 = chg_only$iso3,
    cx = X,
    cy = Y
  )

bbox_tbl <- xy %>%
  left_join(chg_only %>% st_drop_geometry() %>% select(iso3, change_label), by = "iso3") %>%
  mutate(area_score = area_score) %>%
  mutate(
    dx = pmin(base_dx * (1 + 2.2 * area_score), max_dx),
    dy = pmin(base_dy * (1 + 2.2 * area_score), max_dy),
    xmin = cx - dx, xmax = cx + dx,
    ymin = cy - dy, ymax = cy + dy
  ) %>%
  arrange(iso3)

# If there are too many, apply the cap
if (!is.null(top_n_countries)) {
  bbox_tbl <- bbox_tbl %>% slice_head(n = top_n_countries)
}

## Convert bbox rows to sf polygons so the boxes can be drawn on the world map
bbox_sf <- bbox_tbl %>%
  rowwise() %>%
  mutate(
    geometry = st_as_sfc(st_bbox(c(
      xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax
    ), crs = st_crs(map_df)))
  ) %>%
  ungroup() %>%
  st_as_sf()

## ---------------------------------------------------------
## 3) Draw zoom boxes on world maps (1990/2018)
##    - If the boxes look cluttered, tune alpha/line width
## ---------------------------------------------------------
p_world <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "white", linewidth = 0.2) +
  geom_sf(data = df_pair, aes(fill = cluster), color = "white", linewidth = 0.2) +
  geom_sf(data = bbox_sf, fill = NA, color = "black", linewidth = 0.25, alpha = 0.9) +
  scale_fill_manual(values = cluster_cols, name = "Climate cluster") +
  facet_wrap(~year, nrow = 1) +
  coord_sf(crs = 4326, expand = FALSE) +
  theme_void(base_size = 16) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15),
    plot.margin = margin(6, 6, 6, 6)
  )

## ---------------------------------------------------------
## 4) Build country zoom panels (1990 | 2018)
## ---------------------------------------------------------
make_country_zoom <- function(bbox_row, map_df, years_pair, cluster_cols) {
  
  cc <- bbox_row$iso3
  bb <- st_bbox(c(
    xmin = bbox_row$xmin, ymin = bbox_row$ymin,
    xmax = bbox_row$xmax, ymax = bbox_row$ymax
  ), crs = 4326)
  
  df_sub <- map_df %>%
    filter(iso3 == cc, year %in% years_pair) %>%
    mutate(year = factor(year, levels = years_pair))
  
  ggplot() +
    geom_sf(data = world, fill = "grey95", color = "white", linewidth = 0.2) +
    geom_sf(data = df_sub, aes(fill = cluster), color = "white", linewidth = 0.2) +
    scale_fill_manual(values = cluster_cols, drop = FALSE) +
    coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      expand = FALSE
    ) +
    facet_wrap(~year, nrow = 1) +
    theme_void(base_size = 13) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "none",
      plot.margin = margin(2, 2, 2, 2)
    )
}

zoom_list <- lapply(seq_len(nrow(bbox_tbl)), function(i) {
  make_country_zoom(bbox_tbl[i, ], map_df, years_pair, cluster_cols)
})

p_zoom_grid <- wrap_plots(zoom_list, ncol = ncol_zoom)

## ---------------------------------------------------------
## 5) Final composition: world maps (with boxes) + zoom grid
## ---------------------------------------------------------
fig_all <- p_world / p_zoom_grid + plot_layout(heights = c(1, 1.25))

## ---------------------------------------------------------
## 6) Save outputs
## ---------------------------------------------------------
dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)

ggsave(
  file.path(figure_path, "worldmap_with_zoom_boxes_1990_vs_2018.png"),
  p_world,
  width = 12, height = 5.8, dpi = 450
)

ggsave(
  file.path(figure_path, "country_zoom_panels_1990_vs_2018.png"),
  p_zoom_grid,
  width = 14, height = 10, dpi = 400
)

ggsave(
  file.path(figure_path, "world_plus_country_zoom_panels_1990_vs_2018.png"),
  fig_all,
  width = 14, height = 12, dpi = 400
)