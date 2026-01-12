library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(scales)

## =========================================================
## World map: Climate_cluster_5 distribution (1990, 2018)
## - Remove the NA panel: replicate sf(world) rows by year and attach year (avoid using crossing())
## - Countries with missing data or failed matching are shown as NA (grey) within each year panel
## =========================================================

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(countrycode)

sf::sf_use_s2(FALSE)

## ---------------------------
## Paths
## ---------------------------
path <- "/data"
figure_path <- "/figure"
dir.create(figure_path, showWarnings = FALSE, recursive = TRUE)

years_target <- c(1990, 2018)

## ---------------------------
## Load world map
## ---------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(4326) %>%
  mutate(iso3 = iso_a3)

## ---------------------------
## Load climate cluster data
## ---------------------------
data <- fread(file.path(path, "data_merged_quarters.csv"))

df3 <- data %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(year %in% years_target) %>%
  distinct(Country, year, Climate_cluster_5) %>%
  mutate(
    iso3 = countrycode(Country, origin = "country.name", destination = "iso3c"),
    year = factor(year, levels = years_target)
  )

## (Optional) Inspect countries that failed ISO3 matching
unmatched <- df3 %>%
  filter(is.na(iso3)) %>%
  distinct(Country) %>%
  arrange(Country)
print(unmatched)

## ---------------------------
## Merge with world geometry
## ---------------------------
df_map <- world %>%
  select(iso3, geometry) %>%
  left_join(df3, by = "iso3") %>%
  mutate(
    cluster = if_else(is.na(Climate_cluster_5), NA_character_, paste0("C", Climate_cluster_5)),
    year = factor(year, levels = years_target)
  )

## Key idea: replicate the sf object for each year while keeping geometry intact
world_rep <- world %>%
  slice(rep(1:n(), each = length(years_target))) %>%
  mutate(year = factor(rep(years_target, times = nrow(world)), levels = years_target))

df_map2 <- world_rep %>%
  left_join(df3, by = c("iso3", "year")) %>%
  mutate(cluster = if_else(is.na(Climate_cluster_5), NA_character_, paste0("C", Climate_cluster_5)))

cluster_cols <- c(
  "C1" = "#1f78b4",
  "C2" = "#33a02c",
  "C3" = "#fb9a99",
  "C4" = "#e31a1c",
  "C5" = "#6a3d9a"
)

p <- ggplot(df_map2) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.2) +
  scale_fill_manual(values = cluster_cols, na.value = "grey85", name = "Climate cluster") +
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

ggsave(
  filename = file.path(figure_path, "worldmap_climate_cluster_1990_2018.png"),
  plot = p,
  width = 12, height = 5.8, dpi = 450
)