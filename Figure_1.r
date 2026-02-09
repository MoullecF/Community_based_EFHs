###############################################################################
# Title: Variance Partitioning for HMSC Models (Figure 1)
###############################################################################

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
source("ggplot_theme.R")

# Load fitted models
Mpa <- get(load("./Models/model_mpa_thin_4000_samples_250_chains_4.RData"))
Mabu <- get(load("./Models/model_abu_thin_4000_samples_250_chains_4.RData"))

# Variable groups used in variance partitioning
groupnames <- c("Depth", "Temperature", "Salinity", "ChlorophyllA", "Human pressure")
group <- c(
  1, 1, 1,
  2, 2, 2, 2,
  3, 3,
  4, 4,
  5, 5
)

variable_labels <- c(
  "Depth" = "Depth",
  "Temperature" = "Temperature",
  "Salinity" = "Salinity",
  "ChlorophyllA" = "Chlorophyll a",
  "Human.pressure" = "Human pressures",
  "Random..year" = "Year",
  "Random..grid.cell" = "Grid cell"
)

cols <- c("#E69F00", "#56B4E9")

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
compute_vp <- function(model, group, groupnames, variable_labels) {
  # Compute variance partitioning and return a long table with labels applied.
  vp <- computeVariancePartitioning(model, group = group, groupnames = groupnames)
  vp <- data.frame(t(vp$vals))
  vp$Species <- rownames(vp)
  vp <- reshape2::melt(vp)
  vp$Onto <- substr(vp$Species, start = 9, stop = nchar(vp$Species))
  vp <- vp %>% mutate(variable = recode(variable, !!!variable_labels))
  vp
}

summarize_vp <- function(vp) {
  # Summaries used for plot annotations and optional inspection.
  list(
    ordered = vp %>% arrange(variable, desc(value)),
    by_onto = vp %>%
      group_by(variable, Onto) %>%
      summarise(Mean = round(mean(value), 4), SD = round(sd(value), 4)),
    overall = vp %>%
      group_by(variable) %>%
      summarise(Mean = round(mean(value), 2), SD = round(sd(value), 4))
  )
}

plot_vp <- function(vp, vp_summary, title, y_label) {
  ggplot(vp, aes(x = variable, y = value, fill = Onto)) +
    geom_flat_violin(
      position = position_nudge(x = 0.2),
      scale = "width",
      trim = TRUE,
      alpha = 0.6,
      show.legend = TRUE
    ) +
    geom_point(
      aes(color = Onto),
      position = position_jitter(w = 0.1),
      size = 1.25,
      alpha = 0.35,
      show.legend = FALSE
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      aes(fill = Onto),
      color = "black",
      shape = 23,
      size = 3,
      show.legend = FALSE
    ) +
    geom_vline(xintercept = 5.7, linetype = "dashed") +
    geom_text(
      data = vp_summary$by_onto[vp_summary$by_onto$Onto == "adult", ],
      aes(label = paste0(round(Mean, 2), "±", round(SD, 2)), y = -0.05, x = c(1:7)),
      color = "#E69F00",
      size = 4,
      fontface = "bold"
    ) +
    geom_text(
      data = vp_summary$by_onto[vp_summary$by_onto$Onto == "juvenile", ],
      aes(label = paste0(round(Mean, 2), "±", round(SD, 2)), y = -0.08, x = c(1:7)),
      color = "#56B4E9",
      size = 4,
      fontface = "bold"
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols, name = "Life stage", labels = c("Adult", "Juvenile")) +
    labs(y = y_label, x = "") +
    guides(color = "none") +
    theme_bw() +
    scale_x_discrete(expand = expansion(mult = c(0.06, 0.12))) +
    ggtitle(title) +
    theme(
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 16)
    )
}

# -----------------------------------------------------------------------------
# Variance partitioning
# -----------------------------------------------------------------------------
VP.Mpa <- compute_vp(Mpa, group, groupnames, variable_labels)
VP.Mabu <- compute_vp(Mabu, group, groupnames, variable_labels)

vpSummary.Mpa <- summarize_vp(VP.Mpa)
vpSummary.Mabu <- summarize_vp(VP.Mabu)

# -----------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------
gg.VP.Mpa <- plot_vp(
  VP.Mpa,
  vpSummary.Mpa,
  title = "Presence-Absence (PA)",
  y_label = "Proportion of explained variation\n"
)

gg.VP.Mabu <- plot_vp(
  VP.Mabu,
  vpSummary.Mabu,
  title = "Abundance conditional on presence (ABU)",
  y_label = ""
)

# -----------------------------------------------------------------------------
# Combine plots
# -----------------------------------------------------------------------------
VP <- gg.VP.Mpa + gg.VP.Mabu + plot_layout(guides = "collect", widths = c(1, 1))

# ggplot2::ggsave(VP, filename = "./Figures/Figure1.png", width = 50, height = 18, units = "cm", dpi = 400)
