# Community-based Essential Fish Habitats (Mediterranean Sea)

Open-science code and data to model, map, and analyze community-based Essential Fish Habitats (EFHs) across the Western Mediterranean using hierarchical joint species distribution models (HMSC) and hurdle models, followed by spatio-temporal prediction and hotspot analysis.

## Purpose
- Quantify species distributions and abundances with HMSC (presence/absence and abundance components).
- Generate spatio-temporal predictions and hurdle-model products for multiple life stages.
- Identify emerging hotspots/coldspots and evaluate marine protected area (MPA) coverage of EFH patterns.

## Methods and Workflow
1. **Model specification**: Assemble inputs and define HMSC models ([1-Define HMSC models.r](1-Define%20HMSC%20models.r)).
2. **Model fitting**: Fit presence/absence and abundance models (supports HPC workflows) ([2_1-Fit presence absence model.r](2_1-Fit%20presence%20absence%20model.r), [2_2-Fit abundance model.r](2_2-Fit%20abundance%20model.r)).
3. **Diagnostics**: Assess convergence, effective sample sizes, and PSRF ([3-Models convergence and fit.r](3-Models%20convergence%20and%20fit.r)).
4. **Spatial predictions**: Generate posterior samples and means over the spatial grid ([4-Spatial predictions with fitted HMSC.r](4-Spatial%20predictions%20with%20fitted%20HMSC.r)).
5. **Hurdle outputs**: Combine presence/absence and abundance to produce hurdle predictions and derived rasters ([5-Hurdle model predictions.r](5-Hurdle%20model%20predictions.r)).
6. **Hotspot analysis**: Emerging Hotspot Analysis (EHSA) and visualization scripts ([Emerging Hotspot Analysis.r](Emerging%20Hotspot%20Analysis.r), [Figure_1.r](Figure_1.r)–[Figure_S9.r](Figure_S8_S9.r)).

## Inputs and Data
- **Primary inputs**: RData objects and spatial layers in [Inputs_HMSC/](Inputs_HMSC).
  - Species/community matrices, traits, and covariates (e.g., `allData.RData`, `Traits.RData`).
  - Prediction grids and protection layers (shapefiles in `Grid/` and `protectionPLactivities_niv1&2&3&4/`).
- **Model checkpoints**: Fitted models and MCMC artifacts in [Models/](Models).
- **Hurdle prediction stack**: Spatio-temporal raster stack available on Zenodo (DOI: 10.5281/zenodo.18632167). Use [download_zenodo_spatiotemporal.R](download_zenodo_spatiotemporal.R) to place `r_stack_Hurdle_0.05_0_1000_19992021.Rdata` into [Outputs/Spatio_temporal_prediction/](Outputs/Spatio_temporal_prediction).

## Outputs
- Posterior samples and means for presence/absence and abundance: [Outputs/Presence_Absence_posteriors/](Outputs/Presence_Absence_posteriors), [Outputs/Abundance_posteriors/](Outputs/Abundance_posteriors), [Outputs/Presence_Absence_mean_posterior/](Outputs/Presence_Absence_mean_posterior), [Outputs/Abundance_mean_posterior/](Outputs/Abundance_mean_posterior).
- Hurdle predictions (combined): [Outputs/Hurdle_prediction/](Outputs/Hurdle_prediction).
- Spatio-temporal hurdle stack: [Outputs/Spatio_temporal_prediction/](Outputs/Spatio_temporal_prediction).
- EHSA results: [Outputs/EHSA/](Outputs/EHSA).
- Figures and supplementary plots: [Figures/](Figures).

## Installation and Environment
- **R version**: Tested with R (>= 4.x).
- **Core packages**: Listed in [0-Load libraries.r](0-Load%20libraries.r); major dependencies include `Hmsc`, `raster`, `sf`, `ggplot2`, `dplyr`, `zenodor`, and others.
- **Install dependencies**: The loader script installs missing CRAN packages; `zenodor` is installed from GitHub. From the repo root:

```r
source("0-Load libraries.r")
```

If `zenodor` is absent, it is pulled via `remotes::install_github("FRBCesab/zenodor")` inside [download_zenodo_spatiotemporal.R](download_zenodo_spatiotemporal.R).

## Basic Usage
1. Load packages and data paths:
   ```r
   source("0-Load libraries.r")
   ```
2. Define and prepare models:
   ```r
   source("1-Define HMSC models.r")
   ```
3. Fit models (presence/absence and abundance) — adjust settings for local vs HPC:
   ```r
   source("2_1-Fit presence absence model.r")
   source("2_2-Fit abundance model.r")
   ```
4. Run diagnostics and convergence checks:
   ```r
   source("3-Models convergence and fit.r")
   ```
5. Produce spatial predictions and hurdle outputs:
   ```r
   source("4-Spatial predictions with fitted HMSC.r")
   source("5-Hurdle model predictions.r")
   ```
6. Retrieve the published hurdle stack (optional if not already present):
   ```r
   source("download_zenodo_spatiotemporal.R")
   ```
7. Generate hotspot analyses and figures as needed (e.g., `Emerging Hotspot Analysis.r`, `Figure_*.r`).

## Repository Structure
- [0-Load libraries.r](0-Load%20libraries.r): Dependency management and loading.
- [1-Define HMSC models.r](1-Define%20HMSC%20models.r): Model setup and data preparation.
- [2_1-Fit presence absence model.r](2_1-Fit%20presence%20absence%20model.r), [2_2-Fit abundance model.r](2_2-Fit%20abundance%20model.r): Model fitting workflows.
- [3-Models convergence and fit.r](3-Models%20convergence%20and%20fit.r): Diagnostics.
- [4-Spatial predictions with fitted HMSC.r](4-Spatial%20predictions%20with%20fitted%20HMSC.r): Spatial posterior predictions.
- [5-Hurdle model predictions.r](5-Hurdle%20model%20predictions.r): Combine components into hurdle outputs.
- [Emerging Hotspot Analysis.r](Emerging%20Hotspot%20Analysis.r) and [Figure_*.r](Figure_1.r): Hotspot analysis and plotting scripts.
- [Inputs_HMSC/](Inputs_HMSC): Input data and spatial layers.
- [Models/](Models): Fitted model objects and initialization files.
- [Outputs/](Outputs): Generated predictions, EHSA results, and figures (git-ignored intermediates where appropriate).

## Citation
If you use this code or data, please cite:
- **Dataset**: Moullec, F. (2026). *Community-based Essential Fish Habitats in Mediterranean Sea* (Version V1.0.0). Zenodo. https://doi.org/10.5281/zenodo.18632167
- **Code**: This GitHub repository (MoullecF/Community_based_EFHs, main branch). Include commit hash if possible.
- **Associated publication**: Cite the corresponding article when available (see repository or Zenodo record for updates).

## License
This project is released under the GNU General Public License v3.0. See [LICENSE](LICENSE) for details.
