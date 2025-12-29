suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

############################################################
## Typology maps for combined clusters
##
## This script creates world maps for each unique combination
## of climate, diet, and disease clusters in the merged dataset.
##
## Inputs
##   data
##
## Outputs
##   One map per typology combination, saved under
##   figures DD_climate_maps Ck map_Nx_Dy_Ck.png
##
## Assumed project structure
##   project_root
##     data data_merged_quarters.csv
##     figures
############################################################

## Paths
data_path <- file.path("")
out_dir <- file.path("")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## Load merged data
data <- fread(data_path)

## Collect unique typology combinations
data_index <- data %>%
  dplyr::select(iso3, year, GDD_cluster_5, GBD_cluster_5, Climate_cluster_5)

types_ind <- data_index %>%
  dplyr::select(GDD_cluster_5, GBD_cluster_5, Climate_cluster_5) %>%
  unique()

## World map as sf
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_as_sf()

## Store country year rows for each typology if needed later
temp_list <- list()

## Loop over each typology and draw maps
for (i in seq_len(nrow(types_ind))) {
  
  temp <- types_ind[i, ]
  
  ## Subset data for this combination
  temp_data <- data %>%
    filter(
      GDD_cluster_5 == temp$GDD_cluster_5,
      GBD_cluster_5 == temp$GBD_cluster_5,
      Climate_cluster_5 == temp$Climate_cluster_5
    )
  
  ## Save subset into a list for optional downstream use
  list_name <- paste0(
    "N", temp$GDD_cluster_5,
    "_D", temp$GBD_cluster_5,
    "_C", temp$Climate_cluster_5
  )
  temp_list[[list_name]] <- temp_data
  
  ## Countries in this typology
  iso_list <- unique(temp_data$iso3)
  if (length(iso_list) == 0) next
  
  ## Highlighted world subset
  world_highlight <- world %>%
    filter(iso_a3 %in% iso_list)
  
  ## Labels
  label_gdd <- paste0("N", temp$GDD_cluster_5)
  label_gbd <- paste0("D", temp$GBD_cluster_5)
  label_cl  <- paste0("C", temp$Climate_cluster_5)
  
  title_text <- paste(
    "Countries with", label_gdd, "and", label_gbd,
    "within", label_cl
  )
  
  ## Output directory by climate cluster
  cl_dir <- file.path(out_dir, label_cl)
  if (!dir.exists(cl_dir)) dir.create(cl_dir, recursive = TRUE)
  
  file_name <- paste0("map_", label_gdd, "_", label_gbd, "_", label_cl, ".png")
  out_file <- file.path(cl_dir, file_name)
  
  ## Plot
  p <- ggplot() +
    geom_sf(data = world, fill = "grey90", color = "white", linewidth = 0.1) +
    geom_sf(
      data = world_highlight,
      fill = "steelblue",
      color = "white",
      linewidth = 0.2
    ) +
    labs(title = title_text) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  ## Save to file
  ggsave(
    filename = out_file,
    plot = p,
    width = 8,
    height = 4.5,
    dpi = 300
  )
  
  message("Saved: ", out_file)
}

cat("All maps saved to: ", out_dir, "\n")