# 05_classification.r
# Classification modeling SVC on season-level data (AEWSOCC response)

library(tidyverse)
library(here)
library(kernlab)
library(themis)
library(tidymodels)
tidymodels_prefer()

source(here("src", "utils.r"))
set.seed(103)

s_pcs <- read_rds(here("data", "tidy", "s_pcs.rds")) |>
  mutate(AEWSOCC = as_factor(AEWSOCC))

s_rec <- recipe(AEWSOCC ~ ., data=s_pcs) |>
  update_role(ID, new_role="Id") |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

svc_spec <- svm_linear(cost=tune()) |>
  set_engine("kernlab") |>
  set_mode("classification")

svc_wf <- workflow() |>
  add_recipe(s_rec) |>
  add_model(svc_spec)

folds <- vfold_cv(s_pcs, v=7, strata=AEWSOCC)
cost_grid <- grid_regular(cost(), levels = 30)
tune_res <- tune_grid(
  svc_wf, 
  resamples = folds, 
  grid = cost_grid
)

best_cost <- select_best(tune_res, metric="roc_auc")
svc_final <- finalize_workflow(svc_wf, best_cost)

svc_fit <- svc_final |> fit(s_pcs)

# augment(svc_fit, new_data = s_pcs) |>
#   conf_mat(truth = AEWSOCC, estimate = .pred_class)

tbl_svc <- augment(svc_fit, new_data = s_pcs) |> 
      mutate(
        Qualified = ifelse(AEWSOCC==TRUE, "Yes", "No"),
        Score = round(.pred_TRUE, 3)
      ) |>
      arrange(desc(Score)) |>
      select(ID, Qualified, Score) |>
      knitr::kable(type="pipe")

w <- t(svc_fit$fit$fit$fit@xmatrix[[1]]) %*% svc_fit$fit$fit$fit@coef[[1]]

key_pcs <- as_tibble(as.vector(w)) |> 
      mutate(PC = colnames(s_pcs %>% select(starts_with("PC")))) |> 
      rename(weight = value) |> 
      arrange(desc(abs(weight))) |>
      mutate(pct_of_max = abs(weight) / max(abs(weight))) |>
      filter(pct_of_max > 0.333) |>
      pull(PC)

re_s_rec <- recipe(AEWSOCC ~ ., data=s_pcs) |>
  update_role(ID, new_role="Id") |>
  step_select(all_of(c("AEWSOCC", "ID", key_pcs))) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

re_svc_wf <- svc_wf |> update_recipe(re_s_rec)
re_tune <- tune_grid(
  re_svc_wf,
  resamples = folds,
  grid = cost_grid
)

re_best <- select_best(re_tune, metric="roc_auc")
re_final <- finalize_workflow(re_svc_wf, re_best)

re_fit <- re_final |> fit(s_pcs)

# augment(svc_fit, new_data = s_pcs) |>
#   conf_mat(truth = AEWSOCC, estimate = .pred_class)

tbl_re_svc <- augment(re_fit, new_data = s_pcs) |> 
      mutate(
        Qualified = ifelse(AEWSOCC==TRUE, "Yes", "No"),
        Score = round(.pred_TRUE, 3)
      ) |>
      arrange(desc(Score)) |>
      select(ID, Qualified, Score) |>
      knitr::kable(type="pipe")