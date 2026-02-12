###############################################################################
# Title: Trait Contributions to Variance Partitioning (Figure S7)
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

source("./0-Load libraries.r")

# -----------------------------------------------------------------------------
# Load fitted models
# -----------------------------------------------------------------------------

mpa_model <- get(load("./Models/model_mpa_thin_4000_samples_250_chains_4.RData"))
abu_model <- get(load("./Models/model_abu_thin_4000_samples_250_chains_4.RData"))

# -----------------------------------------------------------------------------
# Variance partitioning setup
# -----------------------------------------------------------------------------

group_names <- c(
  "Depth",
  "Temperature bot",
  "Temperature surf",
  "Salinity",
  "ChlorophyllA",
  "Fishing",
  "Gravity"
)

group_index <- c(
  1, 1, 1,  # Depth
  2, 2,     # SBT
  3, 3,     # SST
  4, 4,     # SSS
  5, 5,     # Chla
  6,        # Fishing
  7         # Gravity
)

vp_mpa <- computeVariancePartitioning(mpa_model, group = group_index, groupnames = group_names)
vp_abu <- computeVariancePartitioning(abu_model, group = group_index, groupnames = group_names)

round(vp_mpa$R2T$Y, 2)
round(vp_mpa$R2T$Beta, 2)
round(vp_abu$R2T$Y, 2)
round(vp_abu$R2T$Beta, 2)

# -----------------------------------------------------------------------------
# Trait contributions (% of variance explained by traits per covariate)
# -----------------------------------------------------------------------------

covariates <- c(
  "Intercept", "Depth 1", "Depth 2",
  "SBT 1", "SBT 2",
  "SST 1", "SST 2",
  "SSS 1", "SSS 2",
  "Chla 1", "Chla 2",
  "Fishing", "Gravity"
)

traits_beta_mpa <- data.frame(
  Model     = rep("Presence-Absence", length(covariates)),
  Variable  = covariates,
  value_pct = vp_mpa$R2T$Beta * 100
)

traits_beta_abu <- data.frame(
  Model     = rep("Conditional Abundance", length(covariates)),
  Variable  = covariates,
  value_pct = vp_abu$R2T$Beta * 100
)

# Drop intercept and order by contribution size
traits_beta_mpa <- traits_beta_mpa %>%
  filter(Variable != "Intercept") %>%
  mutate(Variable = factor(Variable)) %>%
  arrange(desc(value_pct)) %>%
  mutate(Variable = factor(Variable, levels = Variable))

traits_beta_abu <- traits_beta_abu %>%
  filter(Variable != "Intercept") %>%
  mutate(Variable = factor(Variable)) %>%
  arrange(desc(value_pct)) %>%
  mutate(Variable = factor(Variable, levels = Variable))

# -----------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------

plot_traits_mpa <- ggplot(traits_beta_mpa, aes(x = Variable, y = value_pct, fill = value_pct)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_c() +
  labs(y = "Proportion %", x = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12)) +
  guides(fill = "none") +
  ggtitle("Presence-Absence")

plot_traits_abu <- ggplot(traits_beta_abu, aes(x = Variable, y = value_pct, fill = value_pct)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_c() +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12)) +
  guides(fill = "none") +
  ggtitle("Conditional Abundance")

# Combine plots
combined_traits <- plot_traits_mpa + plot_traits_abu +
  plot_layout(guides = "collect", widths = c(1, 1))

# ggplot2::ggsave(combined_traits, filename = "./Figures/Figure_S7.png", width = 30, height = 30, units = "cm", dpi = 400)
