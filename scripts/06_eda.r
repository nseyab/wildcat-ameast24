# 05_eda.r
# Find feature contribution to key PCs

library(tidyverse)
library(here)
library(tidymodels)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

pca_rec <- read_rds(here("models", "recipes", "pca.rds"))
key_pcs <- read_rds(here("models", "results", "key_pcs.rds"))

f_map <- pca_rec |> 
  mutate(values = map(rec, ~ extract_loadings_value(.x))) |>
  select(set, values) |>
  unnest(values) |>
  mutate(PC = str_c(set, "_", component)) |>
  select(set, PC, terms, value, sign) |>
  filter(PC %in% str_replace(key_pcs, "PC0", "PC")) |>
  group_by(PC) |>
  mutate(
    abs_est = abs(value),
    pct_of_max = abs_est / max(abs_est)
  ) |>
  ungroup() |>
  select(set, PC, terms, sign, value, pct_of_max)

CTRL_map <- f_map |>
  filter(set == "CTRL") |> select(-set) 

EFF_map <- f_map |>
  filter(set == "EFF") |> select(-set)

VOL_map <- f_map |>
  filter(set == "VOL") |> select(-set)

# |> knitr::kable(type="pipe", digits=4)