# 03_pca.r
# PCA across feature splits

library(tidyverse)
library(here)
library(tidymodels)
library(patchwork)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

pca <- tibble(split=c("eff", "idn", "vol")) |>
  mutate(
    data = map(split, ~ read_rds(here("data", "tidy", paste0("ic_", .x, ".rds")))),
    rec = map(data, ~ create_pca_rec(.x))
  )

write_rds(pca, here("models", "recipes", "pca.rds"))

# plots <- pca |> mutate(plot = map(rec, ~ create_cpve_plot(.x))) |> pull(plot)

# p <- plots[[1]] | plots[[2]] | plots[[3]] + 
#   plot_annotation(title = "Cumulative Proportion of Variance Explained via PCA Components by Feature Set")

# ggsave(here("figs", "p.png"), p, height=5, width=16.5, units="in", dpi="retina")

pca_baked <- pca |>
  mutate(
    prep = map(rec, prep),
    baked = map2(prep, split, ~ {
      prefix <- str_c(str_to_upper(str_sub(.y, 1, 3)), "_PC")
      
      bake(.x, new_data = NULL) |>
        rename_with(
          ~ str_replace(.x, "^PC(\\d{1,2})$", 
            str_c(prefix, str_pad(str_extract(.x, "\\d{1,2}"), width = 2, pad = "0"))
          ),
          .cols = matches("^PC\\d{1,2}$")
        )
    })
  ) |>
  select(split, baked)

write_rds(pca_baked, here("models", "baked", "pca_baked.rds"))
