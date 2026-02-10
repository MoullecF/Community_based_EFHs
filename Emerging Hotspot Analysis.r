
###############################################################################
# Title: Emerging Hotspot Analysis for HMSC Predictions
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

rm(list = ls())

source("./0-Load libraries.R")

# -----------------------------------------------------------------------------
# Inputs
# -----------------------------------------------------------------------------

# Set to c("adult") for a single run, or c("juvenile", "adult") for both.
stages_to_run <- c("adult")

# Load raster stack of hurdle-model outputs
raster_stack <- get(
  load("./Outputs/Spatio_temporal_prediction/r_stack_Hurdle_0.05_0_1000_19992021.Rdata")
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

build_yearly_sf <- function(stage_stack) {
  # Summarize all species layers to yearly abundance per pixel.
  sf_raster <- st_as_sf(rasterToPolygons(stage_stack, dissolve = FALSE))
  layer_names <- setdiff(names(sf_raster), "geometry")
  years <- sub(".*_(\\d{4})", "\\1", layer_names)
  unique_years <- unique(years)

  summed_list <- lapply(unique_years, function(year) {
    year_layers <- grep(paste0("_", year, "$"), layer_names, value = TRUE)
    year_stack <- sf_raster[, colnames(sf_raster) %in% year_layers]

    summed_ab <- year_stack %>%
      mutate(tot_ab = rowSums(across(where(is.numeric)))) %>%
      dplyr::select(tot_ab, geometry)

    summed_ab$tot_ab[summed_ab$tot_ab == 0] <- NA
    outliers_raster <- outliers_mad(summed_ab$tot_ab)
    summed_ab$tot_ab[summed_ab$tot_ab > outliers_raster$limits[2]] <- outliers_raster$limits[2]

    summed_ab$Year <- as.integer(year)

    list_nb <- poly2nb(summed_ab, queen = TRUE)
    empty_nb <- which(card(list_nb) == 0)
    if (length(empty_nb) > 0) {
      summed_ab <- summed_ab[-empty_nb, ]
    }

    summed_ab$pixel_id <- seq_len(nrow(summed_ab))
    summed_ab
  })

  do.call(rbind, summed_list)
}

run_ehsa <- function(stage_name, raster_stack) {
  # Run EHSA for a single life stage and return results.
  stage_stack <- raster::subset(
    raster_stack,
    grep(stage_name, names(raster_stack), value = TRUE)
  )

  summed_sf <- build_yearly_sf(stage_stack)
  spt <- as_spacetime(summed_sf, .loc_col = "pixel_id", .time_col = "Year")
  dplyr::count(spt, Year, pixel_id)
  is_spacetime_cube(spt)

  emerging_hotspot_analysis(
    x = spt,
    .var = "tot_ab",
    threshold = 0.1,
    include_gi = TRUE,
    nsim = 199
  )
}

# -----------------------------------------------------------------------------
# Run EHSA per life stage
# -----------------------------------------------------------------------------

# Note: EHSA runs can take a long time depending on grid size and years.
for (stage in stages_to_run) {
  ehsa <- run_ehsa(stage, raster_stack)
  output_path <- paste0("./Outputs/EHSA/ehsa_", stage, ".rds")
  saveRDS(ehsa, file = output_path)
  message("Saved EHSA output to: ", output_path)
}