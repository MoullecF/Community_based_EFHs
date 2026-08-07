###############################################################################
# Title: Multivariate Environmental Similarity Surfaces (MESS)
###############################################################################

# Literature references:
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00036.x
# https://rdrr.io/rforge/modEvA/man/MESS.html

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

rm(list = ls())

library(modEvA)
library(ggplot2)
library(dplyr)
library(dismo)
library(rnaturalearth)
library(patchwork)

# -----------------------------------------------------------------------------
# Load fitted model and environmental prediction grid
# -----------------------------------------------------------------------------

# environmental prediction grid is the same for both the PA and ABU models,
# so we only need to load it once
mpa_model <- get(load("./Models/model_mpa_thin_4000_samples_250_chains_4.RData"))
load("./Inputs_HMSC/Hindcast_Env_grid_rect0.05_WMED_0_1000.RData")

years <- 1999:2021

# -----------------------------------------------------------------------------
# Compute MESS for each year
# -----------------------------------------------------------------------------

mess_by_year <- vector("list", length(years))

for (i in seq_along(years)) {
  year <- years[i]
  message("Processing ", year)

  grid_env_year <- grid_env %>%
    filter(YEAR == year)

  # Environmental conditions at the observed sites for this year
  obs_env <- data.frame(mpa_model$XData[grep(pattern = year, rownames(mpa_model$XData)), ])
  obs_env$Depth <- -obs_env$Depth
  rownames(obs_env) <- NULL

  # Environmental conditions across the prediction grid for this year
  pred_env <- data.frame(
    Depth   = grid_env_year$Depth,
    SST     = grid_env_year$SST,
    SBT     = grid_env_year$botTemp,
    SSS     = grid_env_year$so,
    Chla    = grid_env_year$chl,
    Fishing = grid_env_year$FPI_tot,
    Gravity = grid_env_year$Gravity
  )

  mess_values <- MESS(V = obs_env, P = pred_env)

  mess_by_year[[i]] <- cbind(
    grid_env_year[, c("X", "Y", "YEAR")],
    MESS = mess_values
  )
}

mess_mpa <- bind_rows(mess_by_year)

# -----------------------------------------------------------------------------
# Summarize MESS across years
# -----------------------------------------------------------------------------

# Per-cell mean/sd/median MESS across all years.
mean_mess_mpa <- mess_mpa %>%
  dplyr::group_by(X, Y) %>%
  dplyr::summarise(mean_TOTAL = mean(MESS.TOTAL, na.rm = TRUE),
    sd_TOTAL = sd(MESS.TOTAL, na.rm = TRUE),
    median_TOTAL = median(MESS.TOTAL, na.rm = TRUE))

# Variables most often responsible for extrapolation (negative MESS)
mess_mpa %>%
  filter(MESS.TOTAL < 0) %>%
  count(MESS.MoD) %>%
  mutate(percent = 100 * n / sum(n))

# Proportion of grid cells with a negative mean MESS value
round(sum(mean_mess_mpa$mean_TOTAL < 0, na.rm = TRUE) / nrow(mean_mess_mpa), 2)

# Per-year summary statistics of MESS values
mess_by_year_summary <- mess_mpa %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(mean_MESS = mean(MESS.TOTAL, na.rm = TRUE),
    median_MESS = median(MESS.TOTAL, na.rm = TRUE),
    pct_extrapolation = 100 * mean(MESS.TOTAL < 0, na.rm = TRUE),
    min_MESS = min(MESS.TOTAL, na.rm = TRUE))

# -----------------------------------------------------------------------------
# Map mean / sd / median MESS values
# -----------------------------------------------------------------------------

world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

# Shared map extent for all three MESS maps (mean, sd, median)
map_xlim <- c(-5.45, 16.15) + c(-0.5, 1.5)
map_ylim <- c(35.75, 44.35) + c(-1, 1.5)

mess_map_theme <- theme(
  panel.grid.major = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
  panel.background = element_rect(fill = "white"),
  plot.margin = unit(c(11, 11, 5.5, 5.5), "pt"),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  legend.position = "right")

mean_mess_map <- ggplot(data = world) +
  geom_tile(data = mean_mess_mpa, aes(x = X, y = Y, fill = mean_TOTAL)) +
  geom_sf(fill = "grey80", color = "grey20") +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", space = "Lab") +
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  labs(fill = "Mean MESS", x = "Longitude", y = "Latitude", tag = "A") +
  guides(shape = guide_legend(override.aes = list(size = 0.2))) +
  mess_map_theme

sd_mess_map <- ggplot(data = world) +
  geom_tile(data = mean_mess_mpa, aes(x = X, y = Y, fill = sd_TOTAL)) +
  geom_sf(fill = "grey80", color = "grey20") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  labs(fill = "St. Dev. MESS", x = "Longitude", y = "Latitude", tag = "B") +
  guides(shape = guide_legend(override.aes = list(size = 0.2))) +
  mess_map_theme

median_mess_map <- ggplot(data = world) +
  geom_tile(data = mean_mess_mpa, aes(x = X, y = Y, fill = median_TOTAL)) +
  geom_sf(fill = "grey80", color = "grey20") +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", space = "Lab") +
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
  labs(fill = "Median MESS", x = "Longitude", y = "Latitude") +
  guides(shape = guide_legend(override.aes = list(size = 0.2))) +
  mess_map_theme

# ggplot2::ggsave(median_mess_map, filename = "./Figures/Median_MESS_map.png", width = 20, height = 15, units = "cm", dpi = 400)

# -----------------------------------------------------------------------------
# Contribution of each predictor to dissimilarity
# -----------------------------------------------------------------------------

ggplot(data = mess_mpa, aes(x = MESS.MoD, y = MESS.TOTAL)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predictor", y = "MESS Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.minor = element_blank())
