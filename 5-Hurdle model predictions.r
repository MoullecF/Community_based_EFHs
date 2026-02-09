# Hurdle model predictions (presence/absence × abundance)

rm(list = ls())
gc()

# Load one fitted model to get species names
load("./Models/model_abu_thin_4000_samples_250_chains_4.RData")
sp.names <- colnames(m$Y)

# Number of posterior samples
nSamples <- 1000

# Confidence interval width (approx. 95% by default)
ci <- function(x, z = 1.96) {
  mean(x, na.rm = TRUE) + z * sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))) -
    (mean(x, na.rm = TRUE) - z * sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
}

# ------------------------------------------------------------------------------
# Inputs and templates
# ------------------------------------------------------------------------------

list.PA <- list.files(path = "./Outputs/Presence_Absence_posteriors", full.names = TRUE)
list.AB <- list.files(path = "./Outputs/Abundance_posteriors", full.names = TRUE)

template <- readRDS(list.PA[1])
preds_t <- array(NA, dim = c(dim(template), length(list.PA)))

# ------------------------------------------------------------------------------
# Hurdle predictions by year
# ------------------------------------------------------------------------------

vec_year <- 1999:2021

for (i in seq_along(vec_year)) {

  year <- vec_year[i]
  cat("Year:", year, "\n")
  t1 <- Sys.time()

  Post_PA <- readRDS(list.PA[grep(pattern = as.character(year), list.PA)])
  Post_AB <- readRDS(list.AB[grep(pattern = as.character(year), list.AB)])

  for (post in 1:nSamples) {
    cat("Post:", post, fill = TRUE)
    # Multiply PA by abundance (undo log with exp)
    preds_t[, , post] <- Post_PA[, , post] * exp(Post_AB[, , post])
  }

  # Point estimates (mean, sd, CI width)
  preds_t_mean <- apply(preds_t, c(1, 2), mean, na.rm = TRUE)
  preds_t_sd <- apply(preds_t, c(1, 2), sd, na.rm = TRUE)
  preds_t_ci <- apply(preds_t, c(1:2), ci)

  saveRDS(preds_t_mean, paste0("./Outputs/Hurdle_prediction/Mean/Mean_Pred_AB_Cond_Pres_", year, ".rds"))
  saveRDS(preds_t_sd, paste0("./Outputs/Hurdle_prediction/SD/SD_Pred_AB_Cond_Pres_", year, ".rds"))
  saveRDS(preds_t_ci, paste0("./Outputs/Hurdle_prediction/Ci/Ci_Pred_AB_Cond_Pres_", year, ".rds"))

  t2 <- Sys.time()
  print(t2 - t1)
  gc()
}

# Generated outputs represent more than 1GB of data. They can be requested from the authors.
