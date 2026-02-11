###############################################################################
# Title: EHSA Hotspot and Coldspot Overlap Maps (Figures 5-6)
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

rm(list = ls())

source("./0-Load libraries.R")
source("./ggplot_theme.R")

# -----------------------------------------------------------------------------
# Global settings and inputs
# -----------------------------------------------------------------------------

# Projected CRS used for area calculations (km^2).
med_albers_crs <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=15 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Load EHSA outputs per life stage.
ehsa_juvenile_90 <- readRDS("./Outputs/EHSA/EHSA_juvenile_90.rds")
ehsa_adult_90 <- readRDS("./Outputs/EHSA/EHSA_adult_90.rds")

classification_order <- c(
  "new hotspot", "consecutive hotspot", "intensifying hotspot",
  "persistent hotspot", "diminishing hotspot", "sporadic hotspot",
  "no pattern detected", "sporadic coldspot", "diminishing colspot",
  "persistent coldspot", "intensifying coldspot", "consecutive coldspot", "new coldspot"
)

# Target spot classes for each group.
hotspot_classes <- c(
  "New Hotspot", "Consecutive Hotspot", "Intensifying Hotspot", "Persistent Hotspot",
  "Diminishing Hotspot", "Sporadic Hotspot"
)

coldspot_classes <- c(
  "New Coldspot", "Consecutive Coldspot", "Intensifying Coldspot", "Persistent Coldspot",
  "Diminishing Coldspot", "Sporadic Coldspot"
)

# Exclude combined classes tied to "no pattern detected".
npd_remove <- c(
  "J.New - A.Npd", "J.Int - A.Npd", "J.Per - A.Npd", "J.dim - A.Npd", "J.Spo - A.Npd",
  "J.Npd - A.Spo", "J.Npd - A.Per", "J.Npd - A.Int", "J.Npd - A.New", "J.Npd - A.Con",
  "J.Con - A.Npd"
)

# Factor level order for hotspot combinations.
hs_levels <- c(
  "J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo",
  "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo",
  "J.dim - A.Int", "J.dim - A.Per", "J.dim - A.Spo",
  "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo",
  "J.Con - A.Per",
  "J.New - A.Int", "J.New - A.Per", "J.New - A.Spo",
  "J.Npd - A.Npd"
)

# Factor level order for coldspot combinations.
cs_levels <- c(
  "J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo", "J.Int - A.Con", "J.Int - A.New",
  "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo", "J.Per - A.Con", "J.Per - A.New",
  "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo", "J.Spo - A.Con", "J.Spo - A.New",
  "J.Con - A.Int", "J.Con - A.Per", "J.Con - A.Spo", "J.Con - A.Con", "J.Con - A.New",
  "J.New - A.Int", "J.New - A.Per", "J.New - A.Spo", "J.New - A.Con", "J.New - A.New",
  "J.Npd - A.Npd"
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

format_ehsa <- function(ehsa_df, order_levels) {
  # Standardize labels and enforce plotting order.
  ehsa_df %>%
    as.data.frame() %>%
    mutate(
      classification = str_to_title(classification),
      classification = factor(classification, levels = str_to_title(order_levels))
    ) %>%
    arrange(classification) %>%
    st_as_sf()
}

shorten_classification <- function(classification, prefix, spot_type) {
  # Short codes keep legends compact and consistent across stages.
  replacements <- c(
    "No Pattern Detected" = paste0(prefix, ".Npd"),
    setNames(paste0(prefix, ".New"), paste0("New ", spot_type)),
    setNames(paste0(prefix, ".Con"), paste0("Consecutive ", spot_type)),
    setNames(paste0(prefix, ".Int"), paste0("Intensifying ", spot_type)),
    setNames(paste0(prefix, ".Per"), paste0("Persistent ", spot_type)),
    setNames(paste0(prefix, ".dim"), paste0("Diminishing ", spot_type)),
    setNames(paste0(prefix, ".Spo"), paste0("Sporadic ", spot_type))
  )
  str_replace_all(classification, replacements)
}

combine_stage_classes <- function(juvenile_sf, adult_sf, drop_patterns) {
  # Join life-stage polygons and build combined class labels.
  st_join(juvenile_sf, adult_sf, largest = TRUE) %>%
    mutate(Classification = paste(classification.x, classification.y, sep = " - ")) %>%
    filter(!str_detect(Classification, "- NA")) %>%
    filter(!Classification %in% drop_patterns) %>%
    mutate(Classification = as.factor(Classification))
}

summarize_class_area <- function(sf_data, spot_label, total_surface_area_km2) {
  # Compute polygon areas and share of the total surface.
  sf_data %>%
    mutate(
      area_km2 = st_area(geometry) %>% units::set_units("km^2") %>% as.numeric(),
      Spot = spot_label
    ) %>%
    st_drop_geometry() %>%
    group_by(Classification, Spot) %>%
    summarise(total_area_km2 = sum(area_km2), n_polygons = n(), .groups = "drop") %>%
    mutate(
      pct_total = (total_area_km2 / total_surface_area_km2) * 100,
      total_area_km2 = round(total_area_km2, 3),
      pct_total = round(pct_total, 3)
    ) %>%
    arrange(desc(total_area_km2))
}

collapse_small_classes <- function(area_table) {
  # Group rare classes to keep the stacked bars readable.
  area_table %>%
    mutate(
      classification_group = ifelse(pct_total < 1, "Others (< 1%)", as.character(Classification))
    ) %>%
    group_by(classification_group, Spot) %>%
    summarize(
      total_area_km2 = sum(total_area_km2),
      n_polygons = sum(n_polygons),
      pct_total = sum(pct_total),
      .groups = "drop"
    ) %>%
    rename(Classification = classification_group) %>%
    mutate(Classification = as.factor(Classification))
}

make_spot_map <- function(spot_df, palette, label_text) {
  # Render a labeled classification map for one spot type.
  ggplot(data = world) +
    geom_sf(data = spot_df, aes(fill = Classification), color = "white", lwd = 0.05, inherit.aes = TRUE) +
    scale_fill_manual(values = palette) +
    geom_sf(fill = "grey90", color = "grey20") +
    coord_sf(
      xlim = c(st_bbox(spot_df)[1] - 0.1, st_bbox(spot_df)[3] + 0.1),
      ylim = c(st_bbox(spot_df)[2] - 0.1, st_bbox(spot_df)[4] + 0.1),
      expand = FALSE
    ) +
    labs(fill = paste0(label_text, "\nClassification"), x = "Longitude", y = "Latitude") +
    guides(shape = guide_legend(override.aes = list(size = 0.2)), fill = guide_legend(ncol = 2)) +
    theme(
      panel.grid.major = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.background = element_rect(fill = "white"),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.position = "right",
      legend.key = element_rect(color = "white")
    ) +
    geom_text(
      data = data.frame(label = label_text, x = -3.4, y = 44),
      aes(label = label, x = x, y = y),
      size = 5,
      fontface = "bold"
    )
}

# -----------------------------------------------------------------------------
# Data preparation
# -----------------------------------------------------------------------------

# Harmonize labels and factor order across stages.
ehsa_juvenile_90 <- format_ehsa(ehsa_juvenile_90, classification_order)
ehsa_adult_90 <- format_ehsa(ehsa_adult_90, classification_order)

ehsa_juvenile_90_proj <- st_transform(ehsa_juvenile_90, crs = med_albers_crs)

# Use juvenile grid to define the total spatial surface.
total_surface_area_km2 <- ehsa_juvenile_90_proj %>%
  distinct(geometry, .keep_all = TRUE) %>%
  st_area() %>%
  sum() %>%
  units::set_units("km^2") %>%
  as.numeric()

# Split the EHSA outputs into hotspot and coldspot subsets.
hs_juv <- ehsa_juvenile_90 %>% filter(classification %in% hotspot_classes)
hs_adu <- ehsa_adult_90 %>% filter(classification %in% hotspot_classes)
cs_juv <- ehsa_juvenile_90 %>% filter(classification %in% coldspot_classes)
cs_adu <- ehsa_adult_90 %>% filter(classification %in% coldspot_classes)

# Prefix life stage for compact combined labels.
hs_juv$classification <- shorten_classification(hs_juv$classification, "J", "Hotspot")
cs_juv$classification <- shorten_classification(cs_juv$classification, "J", "Coldspot")
hs_adu$classification <- shorten_classification(hs_adu$classification, "A", "Hotspot")
cs_adu$classification <- shorten_classification(cs_adu$classification, "A", "Coldspot")

# Join juvenile/adult polygons into combined classes.
hs_df <- combine_stage_classes(hs_juv, hs_adu, npd_remove)
cs_df <- combine_stage_classes(cs_juv, cs_adu, npd_remove)

# Build smooth palettes for each combined legend.
hs_palette_base <- colorRampPalette(brewer.pal(9, "YlGn"))(13)
cs_palette_base <- colorRampPalette(brewer.pal(9, "BuPu"))(22)

hs_legend_col <- c(
  "J.Int - A.Int" = hs_palette_base[13],
  "J.Int - A.Per" = hs_palette_base[12],
  "J.Int - A.Spo" = hs_palette_base[11],
  "J.Per - A.Int" = hs_palette_base[10],
  "J.Per - A.Per" = hs_palette_base[9],
  "J.Per - A.Spo" = hs_palette_base[8],
  "J.dim - A.Spo" = hs_palette_base[7],
  "J.Spo - A.Int" = hs_palette_base[6],
  "J.Spo - A.Per" = hs_palette_base[5],
  "J.Spo - A.Spo" = hs_palette_base[4],
  "J.Con - A.Per" = hs_palette_base[3],
  "J.New - A.Int" = hs_palette_base[2],
  "J.New - A.Spo" = hs_palette_base[1]
)

cs_legend_col <- c(
  "J.Int - A.Int" = cs_palette_base[22],
  "J.Int - A.Per" = cs_palette_base[21],
  "J.Int - A.Spo" = cs_palette_base[20],
  "J.Int - A.Con" = cs_palette_base[19],
  "J.Int - A.New" = cs_palette_base[18],
  "J.Per - A.Int" = cs_palette_base[17],
  "J.Per - A.Per" = cs_palette_base[16],
  "J.Per - A.Spo" = cs_palette_base[15],
  "J.Per - A.Con" = cs_palette_base[14],
  "J.Per - A.New" = cs_palette_base[13],
  "J.Spo - A.Int" = cs_palette_base[12],
  "J.Spo - A.Per" = cs_palette_base[11],
  "J.Spo - A.Spo" = cs_palette_base[10],
  "J.Spo - A.Con" = cs_palette_base[9],
  "J.Spo - A.New" = cs_palette_base[8],
  "J.Con - A.Int" = cs_palette_base[7],
  "J.Con - A.Per" = cs_palette_base[6],
  "J.Con - A.Spo" = cs_palette_base[5],
  "J.Con - A.Con" = cs_palette_base[4],
  "J.New - A.Int" = cs_palette_base[3],
  "J.New - A.Per" = cs_palette_base[2],
  "J.New - A.Spo" = cs_palette_base[1]
)

# Apply factor levels to enforce plotting order.
hs_df$Classification <- factor(hs_df$Classification, levels = hs_levels)
cs_df$Classification <- factor(cs_df$Classification, levels = cs_levels)

# -----------------------------------------------------------------------------
# Figure 5: Hotspot and coldspot maps
# -----------------------------------------------------------------------------

# Build separate hotspot and coldspot panels.
gg.HS <- make_spot_map(hs_df, hs_legend_col, "Hotspots")
gg.CS <- make_spot_map(cs_df, cs_legend_col, "Coldspots")

# Combined map with two independent fill scales.
gg.combined.spot <- ggplot(data = world) +
  geom_sf(data = hs_df, aes(fill = Classification), color = "white", lwd = 0.05, inherit.aes = TRUE) +
  scale_fill_manual("Hotspots Pattern", values = hs_legend_col) +
  guides(fill = guide_legend(ncol = 2)) +
  new_scale_fill() +
  geom_sf(data = cs_df, aes(fill = Classification), color = "white", lwd = 0.05, inherit.aes = TRUE) +
  scale_fill_manual("Coldspots Pattern", values = cs_legend_col) +
  geom_sf(fill = "grey90", color = "grey20") +
  coord_sf(
    xlim = c(st_bbox(hs_df)[1] - 0.1, st_bbox(hs_df)[3] + 0.1),
    ylim = c(st_bbox(hs_df)[2] - 0.1, st_bbox(hs_df)[4] + 0.1),
    expand = FALSE
  ) +
  labs(fill = "Classification", x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(ncol = 2)) +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right",
    legend.key = element_rect(color = "white"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

# Save Figure 5 output.
ggplot2::ggsave(
  gg.combined.spot,
  filename = "./Figures/Figure_5.png",
  width = 20,
  height = 15,
  units = "cm",
  dpi = 400
)

# -----------------------------------------------------------------------------
# Figure 6: Surface-area summaries
# -----------------------------------------------------------------------------

# Project to equal-area CRS for surface computations.
hs_df_proj <- st_transform(hs_df, crs = med_albers_crs)
cs_df_proj <- st_transform(cs_df, crs = med_albers_crs)

# Summarize area by combined class for each spot type.
areas_by_classification_hs <- summarize_class_area(hs_df_proj, "Hotspots", total_surface_area_km2)
areas_by_classification_cs <- summarize_class_area(cs_df_proj, "Coldspots", total_surface_area_km2)

# Collapse rare classes into a single bin.
tab_hs <- collapse_small_classes(areas_by_classification_hs)
tab_cs <- collapse_small_classes(areas_by_classification_cs)

# Merge hotspot and coldspot summaries for plotting.
tab_surf <- bind_rows(tab_hs, tab_cs) %>%
  mutate(Classification = as.factor(Classification))

# -----------------------------------------------------------------------------
# Stacked barplot of surface shares
# -----------------------------------------------------------------------------

# Palette for coldspot class stack.
cold_palette <- c(
  "J.Int - A.Int" = "#4D004B",
  "J.Int - A.Per" = "#60055D",
  "J.Int - A.Spo" = "#740B70",
  "J.Per - A.Int" = "#873C99",
  "J.Per - A.Per" = "#894DA2",
  "J.Per - A.Spo" = "#8A5DAA",
  "J.Spo - A.Int" = "#8C8DC2",
  "J.Spo - A.Per" = "#8F9DC9",
  "J.Spo - A.Spo" = "#96ABD1",
  "Others (< 1%)" = "grey"
)

# Palette for hotspot class stack.
hot_palette <- c(
  "J.Int - A.Per" = "#005C32",
  "J.Per - A.Per" = "#379E54",
  "J.Per - A.Spo" = "#53B466",
  "J.Spo - A.Per" = "#BBE395",
  "J.Spo - A.Spo" = "#D9F0A3",
  "Others (< 1%)" = "grey"
)

gg.surf <- ggplot(tab_surf) +
  # Relevel factors to control the stack order in the legend and bars.
  geom_col(
    data = filter(tab_surf, Spot == "Coldspots"),
    aes(
      x = Spot,
      y = pct_total,
      fill = fct_relevel(
        Classification,
        "Others (< 1%)", "J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo",
        "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo",
        "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo"
      )
    ),
    position = position_stack(reverse = FALSE)
  ) +
  scale_fill_manual(
    values = cold_palette,
    name = "Combined Coldspots Pattern",
    breaks = c(
      "J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo",
      "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo",
      "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo", "Others (< 1%)"
    )
  ) +
  guides(fill = guide_legend(ncol = 2)) +
  new_scale_fill() +
  geom_col(
    data = filter(tab_surf, Spot == "Hotspots"),
    aes(
      x = Spot,
      y = pct_total,
      fill = fct_relevel(
        Classification,
        "Others (< 1%)", "J.Int - A.Per", "J.Per - A.Per", "J.Per - A.Spo",
        "J.Spo - A.Per", "J.Spo - A.Spo"
      )
    ),
    position = position_stack(reverse = FALSE)
  ) +
  scale_fill_manual(
    values = hot_palette,
    name = "Combined Hotspots Pattern",
    breaks = c(
      "J.Int - A.Per", "J.Per - A.Per", "J.Per - A.Spo",
      "J.Spo - A.Per", "J.Spo - A.Spo", "Others (< 1%)"
    )
  ) +
  scale_x_discrete(limits = c("Coldspots", "Hotspots"), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 32),
    breaks = c(0, 10, 20, 30),
    labels = c("0%", "10%", "20%", "30%"),
    expand = c(0, 0)
  ) +
  labs(x = "", y = "Proportion of total surface") +
  HMSC.theme +
  guides(fill = guide_legend(ncol = 2)) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Save Figure 6 output.
ggplot2::ggsave(
  gg.surf,
  filename = "./Figures/Figure_6.png",
  width = 20,
  height = 15,
  units = "cm",
  dpi = 400
)

