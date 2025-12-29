############################################################
## Country-wise visualization of standardized indicators
## - Climate
## - Diet
## - Disease burden
## - Economic indicators
##
## NOTE:
## Update `path` and `figure_path` to match your local setup.
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(writexl)
  library(stringr)
})

## ---------------------------------------------------------
## File paths (modify as needed)
## ---------------------------------------------------------
path <- ""
figure_path <- ""

## ---------------------------------------------------------
## Load merged quarterly data
## ---------------------------------------------------------
data <- fread(file.path(path, "data.csv"))

## ---------------------------------------------------------
## Reshape data into long format by domain
## ---------------------------------------------------------

## Climate indicators
climate_long <- data %>%
  select(iso3, year,
         pm25_z, temp_z, heat_index_z,
         humidity_z, precipitation_z) %>%
  pivot_longer(
    cols = -c(iso3, year),
    names_to = "metric",
    values_to = "z"
  )

## Dietary indicators
diet_long <- data %>%
  select(iso3, year,
         gdd1_z, gdd2_z, gdd3_z, gdd4_z, gdd5_z) %>%
  pivot_longer(
    cols = -c(iso3, year),
    names_to = "metric",
    values_to = "z"
  )

## Disease burden indicators
disease_long <- data %>%
  select(iso3, year,
         gbd1_z, gbd2_z, gbd3_z, gbd4_z, gbd5_z) %>%
  pivot_longer(
    cols = -c(iso3, year),
    names_to = "metric",
    values_to = "z"
  )

## Economic indicators
econ_long <- data %>%
  select(iso3, year,
         gdp_z, trade_z, urban_z) %>%
  pivot_longer(
    cols = -c(iso3, year),
    names_to = "metric",
    values_to = "z"
  )

## ---------------------------------------------------------
## Loop over countries and generate figures
## ---------------------------------------------------------
country_list <- unique(data$iso3)

for (cc in country_list) {
  
  ## -----------------------------
  ## Climate indicators
  ## -----------------------------
  climate_c <- climate_long %>% filter(iso3 == cc)
  
  p_climate <- ggplot(climate_c,
                      aes(x = year, y = z, color = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.6, na.rm = TRUE) +
    labs(
      title = paste0("Standardized Climate Indicators (", cc, ")"),
      x = "Year",
      y = "Z-score",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  
  ggsave(
    file.path(figure_path, paste0("climate_indicators_", cc, ".png")),
    p_climate,
    width = 8,
    height = 5,
    dpi = 200
  )
  
  ## -----------------------------
  ## Dietary indicators
  ## -----------------------------
  diet_c <- diet_long %>% filter(iso3 == cc)
  
  p_diet <- ggplot(diet_c,
                   aes(x = year, y = z, color = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.6, na.rm = TRUE) +
    labs(
      title = paste0("Standardized Dietary Indicators (", cc, ")"),
      x = "Year",
      y = "Z-score",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  
  ggsave(
    file.path(figure_path, paste0("dietary_indicators_", cc, ".png")),
    p_diet,
    width = 8,
    height = 5,
    dpi = 200
  )
  
  ## -----------------------------
  ## Disease burden indicators
  ## -----------------------------
  disease_c <- disease_long %>% filter(iso3 == cc)
  
  p_disease <- ggplot(disease_c,
                      aes(x = year, y = z, color = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.6, na.rm = TRUE) +
    labs(
      title = paste0("Standardized Disease Burden Indicators (", cc, ")"),
      x = "Year",
      y = "Z-score",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  
  ggsave(
    file.path(figure_path, paste0("disease_indicators_", cc, ".png")),
    p_disease,
    width = 8,
    height = 5,
    dpi = 200
  )
  
  ## -----------------------------
  ## Economic indicators
  ## -----------------------------
  econ_c <- econ_long %>% filter(iso3 == cc)
  
  p_econ <- ggplot(econ_c,
                   aes(x = year, y = z, color = metric)) +
    geom_line(linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 1.6, na.rm = TRUE) +
    labs(
      title = paste0("Standardized Economic Indicators (", cc, ")"),
      x = "Year",
      y = "Z-score",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  
  ggsave(
    file.path(figure_path, paste0("economic_indicators_", cc, ".png")),
    p_econ,
    width = 8,
    height = 5,
    dpi = 200
  )
}