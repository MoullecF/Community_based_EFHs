###############################################################################
# Title: Spatiotemporal Trends in Hurdle-Model Abundance (Figure 2)
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

rm(list = ls())
source("./0-Load libraries.R")
source("./ggplot_theme.R")

# -----------------------------------------------------------------------------
# Data preparation
# -----------------------------------------------------------------------------

# Load raster stack of hurdle-model outputs (species, stage, year)
hurdle_stack <- get(
  load("./Outputs/Spatio_temporal_prediction/r_stack_Hurdle_0.05_0_1000_19992021.Rdata")
)

# Parse layer metadata from names
layer_meta <- str_match(names(hurdle_stack), "(.*)_(juvenile|adult)_(\\d{4})")
stage <- layer_meta[, 3]
year <- layer_meta[, 4]

# Aggregate rasters by year and life stage
group_keys <- paste(year, stage, sep = "_")
summed_stack <- stackApply(hurdle_stack, indices = group_keys, fun = sum, na.rm = TRUE)
names(summed_stack) <- unique(group_keys)

cap_outliers_mad <- function(raster_layer) {
  # Replace zeros with NA and cap extreme values using MAD thresholds.
  raster_layer[raster_layer == 0] <- NA
  outliers <- outliers_mad(values(raster_layer))
  raster_layer[raster_layer > outliers$limits[2]] <- outliers$limits[2]
  raster_layer
}

stage_year_layers <- lapply(seq_len(nlayers(summed_stack)), function(i) {
  cap_outliers_mad(summed_stack[[i]])
})
stage_year_stack <- stack(stage_year_layers)

juvenile_stack <- subset(stage_year_stack, subset = paste0("X", 1999:2021, "_juvenile"))
adult_stack <- subset(stage_year_stack, subset = paste0("X", 1999:2021, "_adult"))

# -----------------------------------------------------------------------------
# Trend estimation (Sen's slope and p-values)
# -----------------------------------------------------------------------------

calc_sen_stat <- function(raster_stack, stat) {
  # Compute Sen's slope statistics per cell with minimal data sufficiency.
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

# Convert raster outputs to data frames for plotting
slope_df_juvenile <- as.data.frame(sen_slope_juvenile, xy = TRUE, na.rm = FALSE)
pvalue_df_juvenile <- as.data.frame(sen_pvalue_juvenile, xy = TRUE, na.rm = FALSE)
slope_df_adult <- as.data.frame(sen_slope_adult, xy = TRUE, na.rm = FALSE)
pvalue_df_adult <- as.data.frame(sen_pvalue_adult, xy = TRUE, na.rm = FALSE)

# -----------------------------------------------------------------------------
# Exploratory summaries (kept for continuity with existing workflow)
# -----------------------------------------------------------------------------

range(slope_df_juvenile[pvalue_df_juvenile$layer < 0.05, "layer"], na.rm = TRUE)
hist(slope_df_juvenile[pvalue_df_juvenile$layer < 0.05, "layer"], breaks = 10)
round(
  quantile(
    slope_df_juvenile[pvalue_df_juvenile$layer < 0.05, "layer"],
    probs = seq(0, 1, 0.1),
    na.rm = TRUE
  ),
  2
)
round(quantile(slope_df_juvenile$layer, probs = seq(0, 1, 0.1), na.rm = TRUE), 2)

range(slope_df_adult[pvalue_df_adult$layer < 0.05, "layer"], na.rm = TRUE)
hist(slope_df_adult[pvalue_df_adult$layer < 0.05, "layer"], breaks = 100)
quantile(
  slope_df_adult[pvalue_df_adult$layer < 0.05, "layer"],
  probs = seq(0, 1, 0.1),
  na.rm = TRUE
)
quantile(slope_df_adult$layer, probs = seq(0, 1, 0.1), na.rm = TRUE)

# -----------------------------------------------------------------------------
# Binning and palettes
# -----------------------------------------------------------------------------

bin_sen_slope <- function(slope_df, breaks, labels) {
  slope_df$value_binned <- cut(
    slope_df$layer,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE
  )
  slope_df
}

slope_df_juvenile <- bin_sen_slope(
  slope_df_juvenile,
  breaks = c(-Inf, -35, -16, -10, -6, -3, 0, 1, 2, 4, Inf),
  labels = c(
    "< -35",
    "[-35;-16[",
    "[-16;-10[",
    "[-10;-6[",
    "[-6;-3[",
    "[-3;0[",
    "[0;+1[",
    "[+1;+2[",
    "[+2;+4[",
    "> +4"
  )
)

slope_df_adult <- bin_sen_slope(
  slope_df_adult,
  breaks = c(-Inf, -193, -103, -54, -32, -16, -7, -2, 0, 7, Inf),
  labels = c(
    "< -193",
    "[-193;-103[",
    "[-103;-54[",
    "[-54;-32[",
    "[-32;-16[",
    "[-16;-7[",
    "[-7;-2[",
    "[-2;0[",
    "[0;+7[",
    "> +7"
  )
)

col_neg_juvenile <- c("#CBDEF0", "#ABCFE5", "#81BADA", "#58A1CE", "#3787C0", "#1B69AF", "#084D96")
col_neg_adult <- c("#E0ECF7", "#CBDEF0", "#ABCFE5", "#81BADA", "#58A1CE", "#3787C0", "#1B69AF", "#084D96")
col_pos_juvenile <- c("#FFFFCC", "#FEE692", "#FEBF5A", "#FD8D3C")
col_pos_adult <- c("#FFFFCC", "#FEE692")

col_vec_juvenile <- c(rev(col_neg_juvenile), col_pos_juvenile)
col_vec_adult <- c(rev(col_neg_adult), col_pos_adult)

# -----------------------------------------------------------------------------
# Plotting
# -----------------------------------------------------------------------------

plot_trend_map <- function(slope_df, pvalue_df, color_values) {
  # Use transparency and point overlay to indicate non-significant trends.
  ggplot() +
    geom_tile(
      data = slope_df[pvalue_df$layer < 0.05, ],
      aes(x = x, y = y, fill = value_binned)
    ) +
    geom_tile(
      data = slope_df[pvalue_df$layer > 0.05, ],
      aes(x = x, y = y, fill = value_binned),
      alpha = 0.25
    ) +
    geom_point(
      data = pvalue_df[pvalue_df$layer > 0.05, ],
      aes(x = x, y = y),
      shape = 20,
      size = 0.0001,
      alpha = 1 / 5
    ) +
    geom_sf(data = world, fill = "grey90", color = "grey20") +
    coord_sf(
      xlim = c(range(slope_df$x)[1] - 0.1, range(slope_df$x)[2] + 0.1),
      ylim = c(range(slope_df$y)[1] - 0.1, range(slope_df$y)[2] + 0.1),
      expand = FALSE
    ) +
    labs(fill = "Sen's slope (ind/km^2/year)", x = "Longitude", y = "Latitude") +
    scale_fill_manual(
      values = color_values,
      na.value = "grey50",
      limits = levels(slope_df$value_binned),
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        reverse = FALSE,
        nrow = 1,
        theme = theme(
          legend.text.position = "bottom",
          legend.text = element_text(hjust = 0.5, vjust = 0.65, angle = 0)
        )
      )
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.background = element_rect(fill = "white"),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.position = "bottom",
      legend.key = element_rect(color = "white")
    )
}

juvenile_plot <- plot_trend_map(slope_df_juvenile, pvalue_df_juvenile, col_vec_juvenile)
juvenile_plot
ggplot2::ggsave(
  juvenile_plot,
  filename = "./Figures/Figure_2_juveniles_trend.png",
  width = 30,
  height = 15,
  units = "cm",
  dpi = 400
)

# Black dots indicate non-significant slopes at the 95% level (Mann-Kendall test).
adult_plot <- plot_trend_map(slope_df_adult, pvalue_df_adult, col_vec_adult)
adult_plot
ggplot2::ggsave(
  adult_plot,
  filename = "./Figures/Figure_2_adults_trend.png",
  width = 30,
  height = 15,
  units = "cm",
  dpi = 400
)
