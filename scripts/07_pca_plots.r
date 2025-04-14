# 07_pca_plots.r
# Plots for PCA results
library(tidyverse)
library(here)
library(ggtext)
library(ggbeeswarm)
library(ggforce)
library(ggrepel)
library(patchwork)

source(here("src", "utils.r"))
source(here("src", "vars.r"))

ctrl <- tibble(
  PC = c("PC03", "PC04", "PC01"),
  title = c("Defensive Control", "Tempo & Press", "Shot Quality -vs- Shot Distance"), 
  high = c("Opp More & Quality Chances", "High Tempo, Low Press", "Quality Chances, Ball Control"),
  low = c("Opp Fewer & Lower Quality Chances", "Lower Tempo, Higher Press", "High ASD")
)

eff <- tibble(
  PC = c("PC08", "PC05"),
  title = c("Chance Creation", "Defensive Efficiency"), 
  high = c("Efficient Chance Creation", "Effective Defending"),
  low = c("Wasteful Chances", "Efficient, Desperate Defending & Opp Accurate Shots")
)

vol <- tibble(
  PC = c("PC10", "PC07", "PC11"),
  title = c("Opponent Shot Opportunities", "Defense & Chances", "Directness of Play"), 
  high = c("Opp Chances-- & Aerial Ability", "Clearances, Interceptions, Shots, SoT", "Fouls, PAwS, Clearances, Long Passes & Accuracy"),
  low = c("Opp Chances++ & Serious Fouls", "Defensive Duels Won", "Interceptions, Offensive Duels, Recoveries Mid, Losses Mid")
)

labels <- list(ctrl, eff, vol)

pca_season <- read_rds(here("models", "results", "pca_season.rds"))

plots <- pca_season |>
  mutate(
    labs = labels,
    prep = map2(set, season, ~ prep_for_plots(.x, .y)),
    p1 = map2(labs, prep, ~ plot_key_pcs(.x, .y)),
    p2 = map2(labs, prep, ~ plot_pcs_xy(.x, .y))
  ) |>
  mutate(row_p = map2(p1, p2, ~ .x | .y)) |>
  pull(row_p)

p_final <- wrap_plots(plots, ncol = 1) +
  plot_annotation(
    title = "Which Styles Define America East Teams?",
    subtitle = "Team season averages across key playing style axes within each feature set",
    caption = "&#9651; America East Conference Championship Qualification"
  ) +
  plot_layout(
    widths = c(3, 1),
    heights = c(1, 1, 1)
  )

ggsave(here("figs", "pca_res.png"), p_final, dpi = "retina", width = 12, height = 16)