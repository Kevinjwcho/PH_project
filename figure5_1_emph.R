## =========================================================
## Network change comparison by Climate cluster (for paper figures)
## - Early window: 1990, 1995, 2000
## - Late  window: 2010, 2015, 2018
## - Node size scale fixed across panels
## - Node positions fixed using union layout
## - Edge width legend(|r|) shown ONLY ONCE (C1)
## - Emphasis by COLOR ONLY (no extra width tricks)
## - User can manually set which EDGES/NODES to highlight
## - Highlighted nodes get an outline (stroke) only
## - Layout rows:
##   Row1: C1 | C2
##   Row2: C3 | C4
##   Row3: C3 | C5
## =========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(Hmisc)
  
  library(igraph)
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(patchwork)
})

## ---------------------------
## Paths
## ---------------------------
path <- "/data"
figure_path <- "/figure"

## ---------------------------
## Read data
## ---------------------------
data <- fread(file.path(path, "data_merged_quarters.csv"))

data_network <- data %>%
  dplyr::select(
    Country, year, Climate_cluster_5,
    GDD1, GDD2, GDD3, GDD4, GDD5,
    GBD1, GBD2, GBD3, GBD4, GBD5,
    PM25, MeanT, RH, PR, Heat_index
  )

split_by_cluster <- list(
  C1 = data_network %>% filter(Climate_cluster_5 == 1),
  C2 = data_network %>% filter(Climate_cluster_5 == 2),
  C3 = data_network %>% filter(Climate_cluster_5 == 3),
  C4 = data_network %>% filter(Climate_cluster_5 == 4),
  C5 = data_network %>% filter(Climate_cluster_5 == 5)
)

## ---------------------------
## Variables and windows
## ---------------------------
vars <- c(
  "GDD1","GDD2","GDD3","GDD4","GDD5",
  "GBD1","GBD2","GBD3","GBD4","GBD5",
  "PM25","MeanT","RH","PR","Heat_index"
)

years_early <- c(1990, 1995, 2000)
years_late  <- c(2010, 2015, 2018)

p_cut <- 0.05
r_cut <- 0.3

size_by <- "total"  # "total" or "max"

## =========================================================
## USER CONTROLS: HIGHLIGHT EDGES / NODES
## =========================================================
## 1) Manually highlight specific edges (pairs). Order does not matter.
##    Example:
##      highlight_edges_manual <- tribble(
##        ~from, ~to,
##        "GDD2","GBD3",
##        "PM25","GBD3"
##      )
highlight_edges_manual <- tibble(from = character(), to = character())

## 2) Optionally highlight nodes by name (independent of edges)
##    Example: highlight_nodes_manual <- c("GBD3","PM25")
highlight_nodes_manual <- character()

## 3) If TRUE, nodes connected to ANY highlighted edge are also outlined
outline_nodes_from_highlighted_edges <- TRUE

## 4) If TRUE, use your narrative rule-based highlighting by climate cluster.
##    If FALSE, only manual edges/nodes will be highlighted.
use_narrative_rules <- TRUE

## =========================================================
## 1) Edge table for a window (always returns same columns)
## =========================================================
make_edge_table_window <- function(df, years, vars, p_cut = 0.05, r_cut = 0.3) {
  
  empty_edge <- tibble(
    from = character(),
    to   = character(),
    r    = numeric(),
    p    = numeric(),
    n    = integer()
  )
  
  sub <- df %>%
    filter(year %in% years) %>%
    dplyr::select(all_of(vars)) %>%
    drop_na()
  
  n_use <- nrow(sub)
  if (n_use < 3) return(empty_edge)
  
  rc <- rcorr(as.matrix(sub), type = "pearson")
  r_mat <- rc$r
  p_mat <- rc$P
  
  edges <- expand.grid(from = vars, to = vars, stringsAsFactors = FALSE) %>%
    filter(from < to) %>%
    mutate(
      r = map2_dbl(from, to, ~ r_mat[.x, .y]),
      p = map2_dbl(from, to, ~ p_mat[.x, .y]),
      n = n_use
    ) %>%
    filter(!is.na(r)) %>%
    filter(p < p_cut, abs(r) >= r_cut) %>%
    dplyr::select(from, to, r, p, n)
  
  if (nrow(edges) == 0) empty_edge else edges
}

## =========================================================
## 2) Edge change between windows
## =========================================================
edge_change_windows <- function(df_cluster, vars,
                                years_early, years_late,
                                p_cut = 0.05, r_cut = 0.3) {
  
  e0 <- make_edge_table_window(df_cluster, years_early, vars, p_cut, r_cut) %>%
    rename(r0 = r, p0 = p, n0 = n)
  
  e1 <- make_edge_table_window(df_cluster, years_late, vars, p_cut, r_cut) %>%
    rename(r1 = r, p1 = p, n1 = n)
  
  full_join(e0, e1, by = c("from","to")) %>%
    mutate(
      status = case_when(
        !is.na(r0) & !is.na(r1) ~ "persistent",
        !is.na(r0) &  is.na(r1) ~ "lost",
        is.na(r0) & !is.na(r1)  ~ "gained",
        TRUE ~ NA_character_
      ),
      r_plot = case_when(
        status == "persistent" ~ r1,
        status == "gained"     ~ r1,
        status == "lost"       ~ r0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(status))
}

## =========================================================
## 3) Node degree summary
## =========================================================
node_status_degree <- function(edge_tbl) {
  edge_tbl %>%
    dplyr::select(from, to, status) %>%
    tidyr::pivot_longer(cols = c(from, to), values_to = "node") %>%
    count(node, status, name = "deg") %>%
    tidyr::pivot_wider(names_from = status, values_from = deg, values_fill = 0) %>%
    mutate(
      total_degree = persistent + gained + lost,
      max_degree = pmax(persistent, gained, lost),
      dominant_status = case_when(
        persistent >= gained & persistent >= lost ~ "persistent",
        gained >= persistent & gained >= lost ~ "gained",
        TRUE ~ "lost"
      )
    )
}

## =========================================================
## 4) Build edges for C1–C5
## =========================================================
edges_list <- imap(split_by_cluster, ~ edge_change_windows(.x, vars, years_early, years_late, p_cut, r_cut))

## =========================================================
## 5) Global node-size scaling (shared breaks/limits)
## =========================================================
deg_all <- imap_dfr(edges_list, function(e, nm) {
  if (nrow(e) == 0) {
    return(tibble(cluster = nm, name = vars, total_degree = 0, max_degree = 0, dominant_status = "persistent"))
  }
  node_status_degree(e) %>%
    transmute(cluster = nm, name = node, total_degree, max_degree, dominant_status)
})

global_max_total <- max(deg_all$total_degree, na.rm = TRUE)
global_max_max   <- max(deg_all$max_degree, na.rm = TRUE)

global_max <- if (size_by == "total") global_max_total else global_max_max
size_breaks <- sort(unique(c(0, ceiling(global_max/2), global_max)))

## =========================================================
## 6) Union layout for fixed node positions
## =========================================================
edges_union <- bind_rows(lapply(edges_list, function(e) e %>% dplyr::select(from, to))) %>%
  distinct()

if (nrow(edges_union) == 0) {
  fixed_layout_union <- tibble(
    name = vars,
    x = c(rep(0, 5), rep(1, 5), rep(2, 5)),
    y = c(seq(5, 1), seq(5, 1), seq(5, 1))
  )
} else {
  nodes_union <- tibble(name = sort(unique(c(edges_union$from, edges_union$to))))
  g_union <- tidygraph::tbl_graph(nodes = nodes_union, edges = edges_union, directed = FALSE)
  set.seed(1)
  fixed_layout_union <- ggraph::create_layout(g_union, layout = "fr") %>%
    as_tibble() %>%
    dplyr::select(name, x, y)
}

## =========================================================
## 7) Shared edge width scale (|r|) legend object
## =========================================================
edge_width_scale <- scale_edge_width(
  limits = c(0, 1),
  range = c(0.3, 2.2),
  name = "|Pearson r|",
  guide = guide_legend(
    override.aes = list(
      color = "grey40",
      linetype = "solid",
      alpha = 1
    )
  )
)

## =========================================================
## Helpers: Edge type + highlighting rules
## =========================================================
edge_type_from_vars <- function(from, to) {
  is_gdd_from <- grepl("^GDD", from)
  is_gdd_to   <- grepl("^GDD", to)
  is_gbd_from <- grepl("^GBD", from)
  is_gbd_to   <- grepl("^GBD", to)
  
  is_clim_from <- grepl("^(PM25|MeanT|RH|PR|Heat_index)$", from)
  is_clim_to   <- grepl("^(PM25|MeanT|RH|PR|Heat_index)$", to)
  
  case_when(
    (is_gdd_from & is_gbd_to) | (is_gbd_from & is_gdd_to) ~ "diet_disease",
    (is_gbd_from & is_gbd_to) ~ "disease_disease",
    (is_clim_from | is_clim_to) & (is_gbd_from | is_gbd_to) ~ "climate_disease",
    (is_clim_from | is_clim_to) & (is_gdd_from | is_gdd_to) ~ "climate_diet",
    (is_clim_from | is_clim_to) & !(is_gdd_from | is_gdd_to | is_gbd_from | is_gbd_to) ~ "climate_other",
    TRUE ~ "other"
  )
}

make_edge_key <- function(a, b) {
  # undirected key
  ifelse(a < b, paste0(a, "||", b), paste0(b, "||", a))
}

manual_edge_keys <- make_edge_key(highlight_edges_manual$from, highlight_edges_manual$to)

apply_highlight_rules <- function(edge_tbl, cluster_nm) {
  
  # add edge_type + manual highlight
  out <- edge_tbl %>%
    mutate(
      edge_type = edge_type_from_vars(from, to),
      key = make_edge_key(from, to),
      hl_manual = key %in% manual_edge_keys
    )
  
  if (!use_narrative_rules) {
    return(out %>% mutate(highlight = hl_manual))
  }
  
  # narrative rules based on your paragraph
  # C1–C2: highlight persistent diet–disease (sparse/stable)
  # C3–C4: highlight climate–disease + disease–disease (dense/climate-driven)
  # C5: highlight lost diet–disease (attenuation) + persistent/gained climate–disease and disease–disease
  out %>%
    mutate(
      hl_rule = case_when(
        cluster_nm %in% c("C1","C2") &
          status == "persistent" &
          edge_type == "diet_disease" ~ TRUE,
        
        cluster_nm %in% c("C3","C4") &
          edge_type %in% c("climate_disease", "disease_disease") ~ TRUE,
        
        cluster_nm == "C5" &
          (
            (status == "lost" & edge_type == "diet_disease") |
              (status %in% c("persistent","gained") & edge_type %in% c("climate_disease","disease_disease"))
          ) ~ TRUE,
        
        TRUE ~ FALSE
      ),
      highlight = hl_manual | hl_rule
    )
}

## =========================================================
## 8) Publication plot function (COLOR emphasis only + node outline on highlights)
## =========================================================
make_pub_network_plot_color_emphasis <- function(edge_tbl,
                                                 cluster_nm,
                                                 title = NULL,
                                                 size_by = c("total", "max"),
                                                 fixed_layout,
                                                 size_breaks,
                                                 show_edge_width_legend = TRUE,
                                                 label_size = 5,
                                                 base_size = 16,
                                                 alpha_base = 0.25,
                                                 alpha_hl   = 0.95,
                                                 emphasize_nodes = TRUE) {
  
  size_by <- match.arg(size_by)
  
  if (nrow(edge_tbl) == 0) {
    return(
      ggplot() +
        theme_void(base_size = base_size) +
        ggtitle(title) +
        annotate("text", x = 0, y = 0, label = "No significant edges", size = 5) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  }
  
  ## 1) Highlight rules (must create `highlight`)
  edge_tbl2 <- apply_highlight_rules(edge_tbl, cluster_nm)
  
  ## 2) Node degree from ALL edges (not only highlighted)
  deg_tbl <- node_status_degree(edge_tbl2)
  
  nodes <- tibble(name = sort(unique(c(edge_tbl2$from, edge_tbl2$to)))) %>%
    left_join(deg_tbl, by = c("name" = "node")) %>%
    mutate(
      persistent = ifelse(is.na(persistent), 0, persistent),
      gained = ifelse(is.na(gained), 0, gained),
      lost = ifelse(is.na(lost), 0, lost),
      total_degree = ifelse(is.na(total_degree), 0, total_degree),
      max_degree = ifelse(is.na(max_degree), 0, max_degree),
      dominant_status = ifelse(is.na(dominant_status), "persistent", dominant_status)
    ) %>%
    left_join(fixed_layout %>% select(name, x, y), by = "name")
  
  ## 3) Highlight nodes: manual + endpoints of highlighted edges (optional)
  nodes_from_edges <- character()
  if (outline_nodes_from_highlighted_edges) {
    tmp <- edge_tbl2 %>% dplyr::filter(.data$highlight)
    if (nrow(tmp) > 0) nodes_from_edges <- unique(c(tmp$from, tmp$to))
  }
  
  nodes <- nodes %>%
    mutate(highlight_node = .data$name %in% unique(c(highlight_nodes_manual, nodes_from_edges)))
  
  ## 4) Build edge coordinates from the SAME fixed layout (shared across layers)
  edges_plot <- edge_tbl2 %>%
    transmute(
      from, to, status,
      weight = abs(r_plot),   # |r| for linewidth
      highlight
    ) %>%
    left_join(fixed_layout %>% rename(from = name, x = x, y = y), by = "from") %>%
    left_join(fixed_layout %>% rename(to = name, xend = x, yend = y), by = "to") %>%
    filter(!is.na(x) & !is.na(y) & !is.na(xend) & !is.na(yend))
  
  edges_hl <- edges_plot %>% dplyr::filter(.data$highlight)
  
  ## 5) Colors / linetype
  edge_cols_status <- c(
    persistent = "#2166AC",
    gained     = "#B2182B",
    lost       = "grey70"
  )
  node_cols_status <- edge_cols_status
  lty_map <- c(persistent = "solid", gained = "solid", lost = "22")
  
  node_size_var <- if (size_by == "total") "total_degree" else "max_degree"
  
  ## 6) FIXED PANEL LIMITS: force identical range regardless of highlight layers
  x_rng <- range(fixed_layout$x, na.rm = TRUE)
  y_rng <- range(fixed_layout$y, na.rm = TRUE)
  
  ## 7) Plot
  p <- ggplot()
  
  # Base edges (all)
  p <- p +
    geom_segment(
      data = edges_plot,
      aes(x = x, y = y, xend = xend, yend = yend,
          color = status, linewidth = weight, linetype = status),
      alpha = alpha_base,
      lineend = "round"
    )
  
  # Highlighted edges (subset) - color/alpha only, no legend
  if (nrow(edges_hl) > 0) {
    p <- p +
      geom_segment(
        data = edges_hl,
        aes(x = x, y = y, xend = xend, yend = yend,
            color = status, linewidth = weight, linetype = status),
        alpha = alpha_hl,
        lineend = "round",
        show.legend = FALSE
      )
  }
  
  # Edge scales (match your original shared scale)
  p <- p +
    scale_color_manual(values = edge_cols_status, name = "Edge status") +
    scale_linetype_manual(values = lty_map, name = "Edge status", guide = "none") +
    scale_linewidth(
      limits = c(0, 1),
      range  = c(0.3, 2.2),
      name   = "|Pearson r|"
    ) +
    guides(
      linewidth = guide_legend(
        override.aes = list(color = "grey40", linetype = "solid", alpha = 1)
      )
    )
  
  if (!show_edge_width_legend) {
    p <- p + guides(linewidth = "none")
  }
  
  # Nodes
  p <- p +
    geom_point(
      data = nodes,
      aes_string(x = "x", y = "y", fill = "dominant_status", size = node_size_var),
      shape = 21,
      color = "white",
      stroke = 0.7
    )
  
  # Highlight node outline only (optional) - no legend
  if (emphasize_nodes) {
    p <- p +
      geom_point(
        data = nodes %>% dplyr::filter(.data$highlight_node),
        aes_string(x = "x", y = "y", fill = "dominant_status", size = node_size_var),
        shape = 21,
        color = "black",
        stroke = 1.8,
        show.legend = FALSE
      )
  }
  
  # Node scales + labels + fixed coord
  p <- p +
    scale_fill_manual(values = node_cols_status, name = "Dominant node status") +
    scale_size(
      breaks = size_breaks,
      limits = c(min(size_breaks), max(size_breaks)),
      range  = c(2.5, 10),
      name   = "Node degree",
      guide  = guide_legend(
        override.aes = list(shape = 21, fill = "grey60", color = "white", stroke = 0.7)
      )
    ) +
    geom_text(
      data = nodes,
      aes(x = x, y = y, label = name),
      size = label_size,
      color = "black",
      vjust = 1.3
    ) +
    coord_cartesian(
      xlim = c(x_rng[1] - 0.15 * diff(x_rng), x_rng[2] + 0.15 * diff(x_rng)),
      ylim = y_rng
    )+
    theme_void(base_size = base_size) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = base_size),
      legend.text  = element_text(size = base_size - 1),
      plot.title   = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.margin  = margin(10, 10, 10, 10)
    ) +
    ggtitle(title)
  
  return(p)
}
## =========================================================
## 9) Build plots (Row layout requested)
## =========================================================
title_map <- c(
  C1 = "Temperate Humid (C1)",
  C2 = "Cold Humid (C2)",
  C3 = "Warm Arid (C3)",
  C4 = "Extremely Hot (C4)",
  C5 = "Tropical Humid (C5)"
)

# plots <- imap(edges_list, function(e, nm) {
#   make_pub_network_plot_color_emphasis(
#     edge_tbl = e,
#     cluster_nm = nm,
#     title = title_map[nm],
#     size_by = size_by,
#     fixed_layout = fixed_layout_union,
#     size_breaks = size_breaks,
#     edge_width_scale = edge_width_scale,
#     show_edge_width_legend = (nm == "C1"),
#     label_size = 5,
#     base_size = 16
#   )
# })

plots <- imap(edges_list, function(e, nm) {
  make_pub_network_plot_color_emphasis(
    edge_tbl = e,
    cluster_nm = nm,
    title = title_map[nm],  
    size_by = size_by,
    fixed_layout = fixed_layout_union,
    size_breaks = size_breaks,
    show_edge_width_legend = (nm == "C1"),
    label_size = 5,
    base_size = 16
  )
})

## Requested rows:
## Row1: C1 | C2
## Row2: C3 | C4
## Row3: C3 | C5  (C3 repeated intentionally)
fig_all <- (plots$C1 | plots$C2) /
  (plots$C3 | plots$C4) /
  (plots$C3 | plots$C5) +
  plot_layout(guides = "collect", widths = c(1,1)) &
  theme(legend.position = "right")

fig_all <- fig_all +
  plot_annotation(
    title = "Edge changes (Early: 1990–2000 vs Late: 2010–2018)",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  )

# fig_all <- fig_all &
#   theme(
#     plot.margin = margin(2, 2, 2, 2)
#   )

print(fig_all)

## =========================================================
## 10) Save
## =========================================================
ggsave(
  file.path(figure_path, "edge_change_color_emphasis_rows_C1C2_C3C4_C3C5.pdf"),
  fig_all, width = 12, height = 16
)

ggsave(
  file.path(figure_path, "edge_change_color_emphasis_rows_C1C2_C3C4_C3C5.png"),
  fig_all, width = 14, height = 16, dpi = 300
)

