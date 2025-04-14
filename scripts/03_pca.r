# 03_pca.r
# PCA across feature sets

library(tidyverse)
library(here)
library(tidymodels)
library(patchwork)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

pca <- tibble(set=c("CTRL", "EFF", "VOL")) |>
  mutate(
    data = map(set, ~ read_rds(here("data", "tidy", paste0("ic_", .x, ".rds")))),
    rec = map(data, ~ create_pca_rec(.x))
  )

write_rds(pca, here("models", "recipes", "pca.rds"))

plots <- pca |> mutate(plot = map(rec, ~ plot_cpve(.x))) |> pull(plot)

p <- plots[[1]] | plots[[2]] | plots[[3]] + 
  plot_annotation(title = "Cumulative Proportion of Variance Explained via PCA Components by Feature Set")

ggsave(here("figs", "cpve.png"), p, height=5, width=16.5, units="in", dpi="retina")

pca_baked <- pca |>
  mutate(
    prep = map(rec, ~ prep(.x)),
    baked = map2(prep, set, ~ {
      prefix <- str_c(.y, "_PC")
      
      bake(.x, new_data = NULL) |>
        rename_with(
          ~ str_replace(.x, "^PC(\\d{1,2})$", 
            str_c(prefix, str_pad(str_extract(.x, "\\d{1,2}"), width = 2, pad = "0"))
          ),
          .cols = matches("^PC\\d{1,2}$")
        )
    })
  ) |>
  select(set, baked)

write_rds(pca_baked, here("models", "baked", "pca_baked.rds"))

pca_season <- pca_baked |>
  mutate(
    season = map(baked, ~ .x |> 
      group_by(Team, Year) |>
      summarize(
        across(matches("_PC\\d{2}$"), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"),
        .groups="drop"
      ) |>
      mutate(
        ID = str_c(Team, Year),
        AEWSOCC = factor(ifelse(ID %in% AEWSOCC, "True", "False"), levels=c("True", "False"))
      ) |>
      ungroup() 
    ) 
  ) |>
  select(set, season)

write_rds(pca_season, here("models", "results", "pca_season.rds"))