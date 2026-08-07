###############################################################################
# Title: Cross-Tabulation of EHSA Classes and Sen's Slope Trends
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

rm(list = ls())
source("./0-Load libraries.R")
source("./ggplot_theme.R")

# -----------------------------------------------------------------------------
# Load and prepare hurdle-model raster stack
# -----------------------------------------------------------------------------

hurdle_stack <- get(load("./Outputs/Spatio_temporal_prediction/r_stack_Hurdle_0.05_0_1000_19992021.Rdata"))

# Parse life stage and year out of each layer name
layer_meta <- str_match(names(hurdle_stack), "(.*)_(juvenile|adult)_(\\d{4})")
stage <- layer_meta[, 3]
year <- layer_meta[, 4]

# Sum abundance across species within each year x life-stage combination
group_keys <- paste(year, stage, sep = "_")
summed_stack <- stackApply(hurdle_stack, indices = group_keys, fun = sum, na.rm = TRUE)
names(summed_stack) <- unique(group_keys)

cap_outliers_mad <- function(raster_layer) {
  raster_layer[raster_layer == 0] <- NA
  outliers <- outliers_mad(values(raster_layer))
  raster_layer[raster_layer > outliers$limits[2]] <- outliers$limits[2]
  raster_layer
}

stage_year_stack <- stack(lapply(seq_len(nlayers(summed_stack)), function(i) cap_outliers_mad(summed_stack[[i]])))

juvenile_stack <- subset(stage_year_stack, subset = paste0("X", 1999:2021, "_juvenile"))
adult_stack <- subset(stage_year_stack, subset = paste0("X", 1999:2021, "_adult"))

# -----------------------------------------------------------------------------
# Trend estimation (Sen's slope and p-values)
# -----------------------------------------------------------------------------

calc_sen_stat <- function(raster_stack, stat = c("slope", "pvalue")) {
  stat <- match.arg(stat)
  calc(raster_stack, fun = function(x) {
    if (sum(!is.na(x)) < 2) return(NA)
    sen <- sens.slope(na.omit(x))
    if (stat == "slope") sen$estimates else sen$p.value
  })
}

sen_slope_juvenile <- calc_sen_stat(juvenile_stack, stat = "slope")
sen_pvalue_juvenile <- calc_sen_stat(juvenile_stack, stat = "pvalue")
sen_slope_adult <- calc_sen_stat(adult_stack, stat = "slope")
sen_pvalue_adult <- calc_sen_stat(adult_stack, stat = "pvalue")

build_slope_df <- function(sen_slope, sen_pvalue) {
  slope_df <- as.data.frame(sen_slope, xy = TRUE, na.rm = FALSE)
  names(slope_df)[3] <- "slope"

  pvalue_df <- as.data.frame(sen_pvalue, xy = TRUE, na.rm = FALSE)
  names(pvalue_df)[3] <- "pvalue"

  slope_df$pvalue <- pvalue_df$pvalue
  slope_df$significant <- slope_df$pvalue < 0.05
  slope_df
}

slope_df_juvenile <- build_slope_df(sen_slope_juvenile, sen_pvalue_juvenile)
slope_df_adult <- build_slope_df(sen_slope_adult, sen_pvalue_adult)

# -----------------------------------------------------------------------------
# Load Emerging Hotspot Analysis (EHSA) classifications
# -----------------------------------------------------------------------------

path_ehsa_juvenile <- "./Outputs/EHSA/EHSA_juvenile_90.rds"
path_ehsa_adult <- "./Outputs/EHSA/EHSA_adult_90.rds"

ehsa_juvenile_90 <- readRDS(path_ehsa_juvenile)
ehsa_adult_90 <- readRDS(path_ehsa_adult)

hotspot_classes_lc <- c("new hotspot", "consecutive hotspot", "intensifying hotspot",
  "persistent hotspot", "diminishing hotspot", "sporadic hotspot")

coldspot_classes_lc <- c("new coldspot", "consecutive coldspot", "intensifying coldspot",
  "persistent coldspot", "diminishing coldspot", "sporadic coldspot")

# -----------------------------------------------------------------------------
# Join EHSA classes with Sen's slope significance classes
# -----------------------------------------------------------------------------

build_ehsa_grid <- function(ehsa_sf, hotspot_classes, coldspot_classes) {
  coords <- st_coordinates(st_centroid(ehsa_sf))
  ehsa_sf %>%
    st_drop_geometry() %>%
    mutate(
      x = round(coords[, 1], 2),
      y = round(coords[, 2], 2),
      ehsa_class = case_when(
        classification %in% hotspot_classes ~ "Hotspot",
        classification %in% coldspot_classes ~ "Coldspot",
        TRUE ~ "No pattern")) %>%
    dplyr::select(x, y, ehsa_class)
}

classify_sen_slope <- function(slope_df) {
  slope_df %>%
    mutate(
      x = round(x, 2),
      y = round(y, 2),
      sen_class = case_when(
        is.na(slope) | is.na(pvalue) ~ NA_character_,
        pvalue < 0.05 & slope > 0 ~ "Significant positive",
        pvalue < 0.05 & slope < 0 ~ "Significant negative",
        TRUE ~ "Non-significant")) %>%
    dplyr::select(x, y, sen_class)
}

ehsa_grid_juvenile <- build_ehsa_grid(ehsa_juvenile_90, hotspot_classes_lc, coldspot_classes_lc)
ehsa_grid_adult <- build_ehsa_grid(ehsa_adult_90, hotspot_classes_lc, coldspot_classes_lc)

sen_class_juvenile <- classify_sen_slope(slope_df_juvenile)
sen_class_adult <- classify_sen_slope(slope_df_adult)

# Join on grid coordinates
ehsa_sen_juvenile <- inner_join(ehsa_grid_juvenile, sen_class_juvenile, by = c("x", "y")) %>%
  filter(!is.na(sen_class)) %>%
  mutate(stage = "Juveniles")

ehsa_sen_adult <- inner_join(ehsa_grid_adult, sen_class_adult, by = c("x", "y")) %>%
  filter(!is.na(sen_class)) %>%
  mutate(stage = "Adults")

# -----------------------------------------------------------------------------
# Build cross-tabulation
# -----------------------------------------------------------------------------

sen_levels <- c("Significant positive", "Non-significant", "Significant negative")
ehsa_levels <- c("Hotspot", "Coldspot", "No pattern")

# Proportion (%) of pixels within each EHSA class and stage falling into each Sen's slope significance class
ehsa_sen_crosstab <- bind_rows(ehsa_sen_juvenile, ehsa_sen_adult) %>%
  dplyr::mutate(
    ehsa_class = factor(ehsa_class, levels = ehsa_levels),
    sen_class = factor(sen_class, levels = sen_levels),
    stage = factor(stage, levels = c("Juveniles", "Adults"))) %>%
  dplyr::count(stage, ehsa_class, sen_class, name = "n_pixels") %>%
  dplyr::group_by(stage, ehsa_class) %>%
  dplyr::mutate(pct_pixels = round(100 * n_pixels / sum(n_pixels), 1)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(stage, ehsa_class, sen_class)

ehsa_sen_crosstab_wide <- ehsa_sen_crosstab %>%
  dplyr::select(stage, ehsa_class, sen_class, pct_pixels) %>%
  tidyr::pivot_wider(names_from = sen_class, values_from = pct_pixels, values_fill = 0) %>%
  arrange(stage, ehsa_class)

ehsa_sen_crosstab_wide

# -----------------------------------------------------------------------------
# Plot cross-tabulation as stacked bar plot
# -----------------------------------------------------------------------------

sen_fill_colors <- c("Significant positive" = "#C00000", "Non-significant" = "grey80", "Significant negative" = "#08519C")

ehsa_sen_barplot <- ggplot(ehsa_sen_crosstab, aes(x = ehsa_class, y = pct_pixels, fill = sen_class)) +
  geom_col(position = "stack") +
  facet_wrap(~ stage) +
  scale_fill_manual(values = sen_fill_colors, name = "Sen's slope") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Proportion of pixels (%)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"))

ehsa_sen_barplot
# ggplot2::ggsave(ehsa_sen_barplot, filename = "./Figures/EHSA_Sen_slope_crosstab.png", width = 20, height = 12, units = "cm", dpi = 400)
