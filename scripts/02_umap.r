# 02_umap.r
# UMAP across feature splits

library(tidyverse)
library(here)
library(tidymodels)
library(embed)
library(patchwork)
library(ggrepel)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

umap <- tibble(set=c("FULL", "CTRL", "EFF", "VOL")) |>
  mutate(
    data = map(set, ~ read_rds(here("data", "tidy", paste0("ic_", .x, ".rds")))),
    rec = map(data, ~ create_umap_rec(.x))
  )

write_rds(umap, here("models", "recipes", "umap.rds"))

plots <- umap |> mutate(plot = map2(set, rec, ~ plot_umap(.x, .y))) |> pull(plot)

p <- plots[[1]] / (plots[[2]] | plots[[3]] | plots[[4]]) + plot_annotation(
    title = "Conference Landscape via UMAP by Feature Set",
    subtitle = "Top row: All Features | Bottom row: Control, Efficiency, Volume",
    caption = "&#9651; America East Conference Championship Qualification"
  )

ggsave(here("figs", "umap.png"), p, height=16, width=16, units="in", dpi="retina")