# 02_umap.r
# UMAP at game and season levels

library(tidyverse)
library(here)
library(tidymodels)
library(embed)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

data <- read_rds(here("data", "tidy", "ic_data.rds"))

umap_rec <- recipe(~., data=data) |>
  update_role(ID, Team, Year, Date, Opp, new_role="id") |>
  update_role(Points, PResult, new_role="outcome") |>
  step_YeoJohnson() |>
  step_normalize(all_numeric_predictors()) |>
  step_umap(all_numeric_predictors(), neighbors=7, min_dist=0.14) |>
  prep()

write_rds(umap_rec, here("output", "umap_rec.rds"))