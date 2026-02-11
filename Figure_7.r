###############################################################################
# Title: MPA Coverage of EHSA Hotspots (Figure 7)
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

rm(list = ls())

source("./0-Load libraries.r")
source("./ggplot_theme.R")

# -----------------------------------------------------------------------------
# Load spatial data
# -----------------------------------------------------------------------------

# Marine protected areas with protection levels minimally --> Fully.
mpa_polygons <- st_read("./Inputs_HMSC/protectionPLactivities_niv1&2&3&4/protectionPLactivities_niv1&2&3&4.shp")
# Fix any invalid geometries (self-intersections, etc.).
mpa_polygons <- st_make_valid(mpa_polygons)

# Combined hotspot and coldspot EHSA outputs.
df_spots <- readRDS("./Outputs/EHSA/Combined_HS_CS.RDS")

# Align CRS for spatial operations.
df_spots <- st_transform(df_spots, st_crs(mpa_polygons))

# -----------------------------------------------------------------------------
# Aggregate protection level summaries
# -----------------------------------------------------------------------------

# Calculate total hotspot/coldspot surface in each protection category.
# Intersect spots with MPAs and compute total areas by protection level.
protected_intersections <- st_intersection(
  df_spots %>% dplyr::select(Spot),  # Keep only spot type column.
  mpa_polygons %>% dplyr::select(protctn)  # Keep only protection level.
) %>%
  mutate(area = as.numeric(st_area(geometry)))

# Aggregate by spot type and protection level.
protected_areas <- protected_intersections %>%
  st_drop_geometry() %>%
  group_by(Spot, protctn) %>%
  summarise(total_area = sum(area), .groups = "drop") %>%
  mutate(protctn = as.character(protctn))

# Compute areas outside any MPA (unprotected).
unprotected_areas <- st_difference(df_spots, st_union(mpa_polygons)) %>%
  mutate(
    area = as.numeric(st_area(geometry)),
    protctn = "Unprotected"
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(Spot, protctn, total_area = area)

# Combine protected and unprotected summaries and compute proportions.
summary_all <- bind_rows(protected_areas, unprotected_areas) %>%
  group_by(Spot) %>%
  mutate(prop = (total_area / sum(total_area, na.rm = TRUE)) * 100) %>%
  group_by(Spot, protctn, .add = TRUE) %>%
  summarise(
    total_area = sum(total_area, na.rm = TRUE),
    prop = round(sum(prop, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(protctn = factor(protctn, levels = c("Unprotected", "1", "2", "3", "4"))) %>%
  # Recode numeric protection levels to meaningful labels.
  mutate(protctn = fct_recode(
    protctn,
    "Minimally" = "1",
    "Lightly" = "2",
    "Highly" = "3",
    "Fully" = "4"
  ))

# -----------------------------------------------------------------------------
# Bar plots of protection coverage
# -----------------------------------------------------------------------------

# Color scale matching protection categories.
protection_palette <- c(
  "Unprotected" = "gray90",
  "Minimally" = "#bdd7e7",
  "Lightly" = "#6baed6",
  "Highly" = "#3182bd",
  "Fully" = "#08519c"
)

# Horizontal bar chart showing hotspot surface by protection level.
bar_hotspots <- ggplot(
  summary_all[summary_all$Spot == "Hotspots", ],
  aes(x = prop, y = protctn, fill = protctn)
) +
  geom_bar(stat = "identity", colour = "white", linewidth = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = protection_palette) +
  scale_x_continuous(
    breaks = c(0, 3, 19, 79),
    labels = c("0", "3%", "19%", "79%"),
    limits = c(0, 80),
    expand = c(0, 0)
  ) +
  labs(x = "Proportion of hotspot surface", y = element_blank()) +
  annotate("text", x = 9, y = 5, label = "Fully protected", size = 11 / .pt) +
  annotate("text", x = 11, y = 4, label = "Highly protected", size = 11 / .pt) +
  annotate("text", x = 14, y = 3, label = "Lightly protected", size = 11 / .pt) +
  annotate("text", x = 32, y = 2, label = "Minimally protected", size = 11 / .pt) +
  annotate("text", x = 65, y = 1, label = "Unprotected", size = 11 / .pt) +
  HMSC.theme +
  theme(
    axis.title.x = element_text(face = "plain", size = 14),
    axis.text.x = element_text(face = "plain", size = 12),
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text.y = element_blank(),
    plot.margin = unit(c(0, 0.5, 0, 0), "cm")
  )

# -----------------------------------------------------------------------------
# Per-cell protection coverage
# -----------------------------------------------------------------------------

# Compute proportional MPA coverage within each hotspot/coldspot cell.
# Assign unique IDs and compute original cell areas.
df_spots <- df_spots %>%
  mutate(id = row_number(), spot_area = st_area(geometry))

# Intersect each spot cell with MPAs and compute proportional coverage.
cell_intersections <- st_intersection(df_spots, mpa_polygons) %>%
  mutate(
    intersection_area = st_area(geometry),  # Area of overlap.
    prop_cover = as.numeric(intersection_area / spot_area)  # Fraction covered.
  )

# Pivot to wide format: one column per protection level.
# If multiple MPA polygons overlap a cell, sum their contributions.
coverage_wide <- cell_intersections %>%
  st_set_geometry(NULL) %>%
  group_by(id, protctn) %>%
  summarise(proportion = sum(prop_cover) * 100, .groups = "drop") %>%
  pivot_wider(names_from = protctn, values_from = proportion, values_fill = 0)

# Merge coverage back into spatial dataframe.
df_spots_with_cover <- df_spots %>%
  left_join(coverage_wide, by = "id")

# -----------------------------------------------------------------------------
# Prepare hotspot cells for spatial visualization
# -----------------------------------------------------------------------------

# Filter hotspots and compute unprotected proportion.
hotspots <- df_spots_with_cover %>%
  filter(Spot == "Hotspots")

# Identify new protection columns added by the coverage join.
protctn_cols <- setdiff(names(hotspots), names(df_spots))

# Remainder after summing all protected proportions.
hotspots <- hotspots %>%
  mutate(unprotected = 100 - rowSums(across(all_of(protctn_cols)), na.rm = TRUE))

# Reshape to long format for mapping by protection category.
# One row per cell × category combination.
hotspots_long <- hotspots %>%
  dplyr::select(id, geometry, all_of(protctn_cols), unprotected) %>%
  pivot_longer(
    cols = c(all_of(protctn_cols), unprotected),
    names_to = "category",
    values_to = "proportion"
  ) %>%
  mutate(
    category = recode(
      category,
      "1" = "Minimally",
      "2" = "Lightly",
      "3" = "Highly",
      "4" = "Fully",
      "unprotected" = "Unprotected"
    ),
    proportion = pmax(proportion, 0, na.rm = TRUE)  # Clamp negatives to 0.
  ) %>%
  st_transform(4326)  # WGS84 for display.

# -----------------------------------------------------------------------------
# Helper function for protection category maps
# -----------------------------------------------------------------------------

make_protection_map <- function(data, category_name, subtitle_color, x_label, y_label, show_legend) {
  # Render spatial coverage for a single protection level.
  # Filters data to one category and maps % coverage per cell.
  cat_data <- data[data$category == category_name, ]
  
  ggplot(cat_data) +
    geom_sf(aes(fill = proportion), color = NA, lwd = 0.05, show.legend = show_legend) +
    geom_sf(data = world, fill = "grey90", color = "grey20") +
    coord_sf(
      xlim = c(st_bbox(data)[1] - 0.1, st_bbox(data)[3] + 0.15),
      ylim = c(st_bbox(data)[2] - 0.1, st_bbox(data)[4] + 0.15),
      expand = FALSE
    ) +
    scale_fill_viridis_c(
      option = "viridis",
      name = "Percentage of cell area",
      labels = function(x) paste0(x, "%")
    ) +
    labs(x = x_label, y = y_label, subtitle = category_name) +
    guides(
      shape = guide_legend(override.aes = list(size = 0.2)),
      fill = guide_colorbar(title.position = "top")
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.background = element_rect(fill = "white"),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.key = element_rect(color = "white"),
      plot.margin = unit(c(0, 0, 0, 0), "pt"),
      plot.subtitle = element_text(face = "bold", color = subtitle_color, size = 14)
    )
}

# -----------------------------------------------------------------------------
# Individual protection category maps
# -----------------------------------------------------------------------------

# Build five maps (one per protection category) and customize legend placement.
# Each map shows % of cell area under that protection level.
map_unprotected <- make_protection_map(
  hotspots_long, "Unprotected", "gray", element_blank(), "Latitude", TRUE
) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.height = unit(16, "pt"),
    legend.key.width = unit(70, "pt")
  )

map_minimally <- make_protection_map(
  hotspots_long, "Minimally", "#bdd7e7", element_blank(), element_blank(), FALSE
)

map_lightly <- make_protection_map(
  hotspots_long, "Lightly", "#6baed6", element_blank(), element_blank(), FALSE
)

map_highly <- make_protection_map(
  hotspots_long, "Highly", "#3182bd", "Longitude", "Latitude", FALSE
)

map_fully <- make_protection_map(
  hotspots_long, "Fully", "#08519c", "Longitude", element_blank(), FALSE
)

# -----------------------------------------------------------------------------
# Composite figure
# -----------------------------------------------------------------------------

# Combine five spatial maps + one bar chart in a multi-panel layout.
full_hs_plot <- map_unprotected + map_minimally + map_lightly +
  map_highly + map_fully + bar_hotspots +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.box = "horizontal",
    legend.margin = margin()
  )

# Save output as high-resolution PNG.
ggplot2::ggsave(
  full_hs_plot,
  filename = "./Figures/Figure_7.png",
  width = 45,
  height = 20,
  units = "cm",
  dpi = 400
)
