# 08_loading_plot.r
# Plots for PCA results
library(tidyverse)
library(scico)
library(ggtext)

prep <- read_rds(here("models", "recipes", "pca.rds")) |>
  filter(set == "VOL") |>
  pull(rec) |>
  first() |>
  prep()

key_pcs <- read_rds(here("models", "results", "key_pcs.rds"))[6:8] |>
  str_replace("VOL_", "") |>
  str_replace("PC0", "PC")

labs <- tibble(
  PC = c("PC10", "PC7", "PC11"),
  title = c("Opponent Shot Opportunities", "Defense & Chances", "Directness of Play")
)
facet_labeller <- as_labeller(setNames(str_wrap(labs$title, width=36), labs$PC))

p_loadings <- tidy(prep, 3) |>
  filter(component %in% key_pcs) |>
  mutate(component = factor(component, levels=key_pcs)) |>
  arrange(component) |>
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  scale_fill_scico_d(palette="oslo", begin=0.3, end=0.7) +
  # scale_fill_viridis_d(option="plasma", end=0.8) +
  facet_wrap(~component, nrow = 1, labeller = facet_labeller) +
  labs(
    title = "Volume Set Principal Components (PC)",
    subtitle = "Top 3 components that best distinguish tournament and non-tournament teams (|effect size| ≥ 0.33)",
    x = "Feature Contribution to PC",
    y = NULL
  ) +
  theme(
    axis.text.y = element_text(family = "ssp", size = 28, color="#263645"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(angle=0, size=40, lineheight=0.4)
  )

ggsave(here("figs", "loadings.png"), p_loadings, height=16, width=16, units="in", dpi="retina")