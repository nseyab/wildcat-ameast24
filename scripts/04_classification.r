# 04_classification.r
# Classification modeling on season-level data (AEWSOCC + Table response)

library(tidyverse)
library(here)
library(MASS)
library(kernlab)
library(discrim)
library(tidymodels)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

models <- tibble(
  model = c("lda", "svm_linear"),
  spec = list(
    discrim_linear() |> set_engine("MASS") |> set_mode("classification"),
    svm_linear(cost=tune(), margin=tune()) |> set_engine("kernlab") |> set_mode("classification")
  )
)

metric <- yardstick::metric_set(accuracy)

class <- read_rds(here("models", "results", "pca_season.rds")) |>
  mutate(season = map(season, ~ select(.x, -ends_with("_sd")))) |>
  mutate(spec = list(models)) |>
  unnest(spec) |>
  mutate(
    rec = map2(season, model, ~ create_class_rec(.x, .y)),
    fit = map2(spec, rec, ~ extract_fit(.x, .y)),
    preds = pmap(list(season, model, fit), add_preds),
    acc = map(preds, ~ metric(.x, truth=.truth, estimate=.pred))
  )

write_rds(class, here("models", "results", "class_res.rds"))

