# 04_pca.r
# Principle Component Analysis at game level aggregate to season

library(tidyverse)
library(here)
library(tidymodels)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

ic_data <- read_rds(here("data", "tidy", "ic_data.rds"))

pca_rec <- recipe(~., data=ic_data) |>
  update_role(ID, Team, Year, Date, Opp, new_role="id") |>
  update_role(Points, PResult, new_role="outcome") |>
  step_YeoJohnson() |>
  step_normalize(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), threshold=0.8, id="pca") |>
  prep()

pca_juice <- pca_rec |> juice()

s_pcs <- pca_juice |>
  group_by(Team, Year) |>
  summarize(across(starts_with("PC"), mean), .groups="drop") |>
  mutate(ID = str_c(Team, Year), .before=Team) |>
  mutate(AEWSOCC = ifelse(ID %in% AEWSOCC, TRUE, FALSE), .before=Team) |>
  select(-c(Team, Year))

write_rds(pca_rec, here("output", "pca_rec.rds"))
write_rds(s_pcs, here("data", "tidy", "s_pcs.rds"))