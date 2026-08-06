### Multivariate Environmental Similarity Surfaces - MESS ###

# Literature references:
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00036.x
# https://rdrr.io/rforge/modEvA/man/MESS.html
# https://link.springer.com/article/10.1007/s10531-016-1105-y

# Load required libraries
library(modEvA)
library(ggplot2)
library(dplyr)
library(dismo)
library(rnaturalearth)
library(patchwork)

rm(list = ls())

# Load the HMSC models and environmental grid
Mpa <- get(load("./Models/model_mpa_thin_4000_samples_250_chains_4.RData"))
load("./Inputs_HMSC/Hindcast_Env_grid_rect0.05_WMED_0_1000.RData")

# Years to process
years <- 1999:2021

# Empty list to store results
mess_list_Mpa <- vector("list", length(years))

for(i in seq_along(years)){

  year <- years[i]

  message("Processing ", year)

  # Prediction grid for this year
  grid_env_year <- grid_env %>%
    filter(YEAR == year)

  # Environmental data used to fit the PA model
  dat.obs.Mpa <- data.frame(Mpa$XData[grep(pattern = year, rownames(Mpa$XData)), ])
  dat.obs.Mpa$Depth <- -dat.obs.Mpa$Depth
  rownames(dat.obs.Mpa) <- NULL

  # Environmental data used for prediction
  dat.pred <- data.frame(
    Depth    = grid_env_year$Depth,
    SST      = grid_env_year$SST,
    SBT      = grid_env_year$botTemp,
    SSS      = grid_env_year$so,
    Chla     = grid_env_year$chl,
    Fishing  = grid_env_year$FPI_tot,
    Gravity  = grid_env_year$Gravity)

  # Compute MESS
  mess_values_Mpa <- MESS(V = dat.obs.Mpa, P = dat.pred)

  # Store results together with coordinates and year
  mess_list_Mpa[[i]] <- cbind(
    grid_env_year[, c("X", "Y", "YEAR")],
    MESS = mess_values_Mpa)
  
}

# Final table
mess_Mpa <- bind_rows(mess_list_Mpa)

# Mean MESS values across years
mean_mess_Mpa <- mess_Mpa %>%
  group_by(X, Y) %>%
  summarise(mean_TOTAL = mean(MESS.TOTAL, na.rm = TRUE),
            sd_TOTAL = sd(MESS.TOTAL, na.rm = TRUE),
            median_TOTAL = median(MESS.TOTAL, na.rm = TRUE))

# Variables driving extrapolation
mess_Mpa %>%
  filter(MESS.TOTAL < 0) %>%
  count(MESS.MoD) %>%
  mutate(percent = 100*n/sum(n))

# Proportion of grid cells with negative mean MESS values
round(sum(mean_mess_Mpa$mean_TOTAL < 0, na.rm = TRUE) / nrow(mean_mess_Mpa), 2)

# Compute summary statistics for MESS values by year
tab_mess_Mpa <- mess_Mpa %>%
  group_by(YEAR) %>%
  summarise(
    mean_MESS = mean(MESS.TOTAL, na.rm = TRUE),
    median_MESS = median(MESS.TOTAL, na.rm = TRUE),
    pct_extrapolation = 100 * mean(MESS.TOTAL < 0, na.rm = TRUE),
    min_MESS = min(MESS.TOTAL, na.rm = TRUE))

# Plot the mean MESS values
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

mean_MESS_map <- ggplot(data = world) +
  geom_tile(data = mean_mess_Mpa, aes(x = X, y = Y, fill = mean_TOTAL)) +
  geom_sf(fill = "grey80", color = "grey20") +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", space = "Lab" ) +
  coord_sf(xlim = c(-5.45, 16.15) + c(-0.5,1.5), ylim = c(35.75, 44.35) + c(-1, 1.5), expand = FALSE) +
  labs(fill = "Mean MESS", x = "Longitude", y = "Latitude", tag = "A") +
  guides(shape = guide_legend(override.aes = list(size = 0.2))) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        plot.margin = unit(c(11, 11, 5.5, 5.5), "pt"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12), legend.position = "right")

# Plot the standard deviation of MESS values
sd_MESS_map <- ggplot(data = world) +
  geom_tile(data = mean_mess_Mpa, aes(x = X, y = Y, fill = sd_TOTAL)) +
  geom_sf(fill = "grey80", color = "grey20") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_sf(xlim = c(-5.45, 16.15) + c(-0.5,1.5), ylim = c(35.75, 44.35) + c(-1, 1.5), expand = FALSE) +
  labs(fill = "St. Dev. MESS", x = "Longitude", y = "Latitude", tag = "B") +
  guides(shape = guide_legend(override.aes = list(size = 0.2))) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), # "aliceblue"
        plot.margin = unit(c(11, 11, 5.5, 5.5), "pt"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12), legend.position = "right")

# Plot median MESS values across years
median_MESS_map <- ggplot(data = world) +
  geom_tile(data = mean_mess_Mpa, aes(x = X, y = Y, fill = median_TOTAL)) +
  geom_sf(fill = "grey80", color = "grey20") +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", space = "Lab" ) +
  coord_sf(xlim = c(-5.45, 16.15) + c(-0.5,1.5), ylim = c(35.75, 44.35) + c(-1, 1.5), expand = FALSE) +
  labs(fill = "Median MESS", x = "Longitude", y = "Latitude") +
  guides(shape = guide_legend(override.aes = list(size = 0.2))) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        plot.margin = unit(c(11, 11, 5.5, 5.5), "pt"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12), legend.position = "right")

# ggplot2::ggsave(median_MESS_map, filename = "./Figures/Median_MESS_map.png", width = 20, height = 15, units = "cm", dpi = 400)

# ggplot environmental variables responsible for dissimilarity
ggplot(data = mess_Mpa, aes(x = MESS.MoD, y = MESS.TOTAL)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predictor", y = "MESS Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size =12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank())
