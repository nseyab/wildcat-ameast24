# 05_reduced.r
# Reduced LDA classification modeling on season-level data (AEWSOCC response)

library(tidyverse)
library(here)
library(MASS)
library(tidymodels)
tidymodels_prefer()

source(here("src", "utils.r"))
source(here("src", "vars.r"))
set.seed(103)

# class |> 
#   unnest(acc) |> 
#   select(set, model, .metric, .estimate)

class <- read_rds(here("models", "results", "class_res.rds"))
season <- read_rds(here("models", "results", "pca_season.rds")) |>
  mutate(season = map(season, ~ select(.x, -ends_with("_sd"))))

loadings <- class |> 
  filter(model == "lda") |> 
  select(set, fit) |> 
  mutate(
    lda = map(fit, ~ .x$fit$fit$fit), 
    loadings = map(lda, ~ .x$scaling |>
      as_tibble(rownames = "PC") |>
      rename(estimate = LD1) |>
      mutate(
        abs_est = abs(estimate),
        pct_of_max = abs_est / max(abs_est)
      ) |>
      arrange(desc(abs_est))
    )
  ) |> 
  pull(loadings)

tau <- c(0.5, 0.333, 0.25, 0.2, 0)

reduced_class <- loadings |> 
  map2(season$season, ~ map_dfr(tau, function(thresh) {
    
    pcs <- .x |> filter(pct_of_max >= thresh) |> pull(PC)

    data <- .y |> select(all_of(c("ID", "Team", "Year", "AEWSOCC", pcs)))

    lda_fit <- discrim_linear() |> set_engine("MASS") |> 
      fit(AEWSOCC ~ ., data = data)

    preds <- data |> 
      mutate(.pred = predict(lda_fit, new_data = data)$.pred_class)

    acc_train <- yardstick::accuracy(preds, truth = AEWSOCC, estimate = .pred)$.estimate

    resamples <- vfold_cv(data, v = 7, repeats = 6, strata = AEWSOCC)

    rec <- recipe(AEWSOCC ~ ., data = data) |>
      update_role(ID, Team, Year, new_role = "id")

    lda_wf <- workflow() |> 
      add_recipe(rec) |> 
      add_model(discrim_linear() |> set_engine("MASS"))

    cv_fit <- lda_wf |> fit_resamples(
      resamples = resamples,
      metrics = metric_set(accuracy)
    )

    acc_cv <- collect_metrics(cv_fit) |> 
      filter(.metric == "accuracy") |> 
      pull(mean)

    tibble(
      threshold = thresh,
      n_pcs = length(pcs),
      accuracy_train = acc_train,
      accuracy_cv = acc_cv
    )
  }))

comparison <- tibble(set = c("Control", "Efficiency", "Volume"), res = reduced_class) |> unnest(res)
reduced_pcs <- tibble(set = c("Control", "Efficiency", "Volume"), LD = loadings, best_n = c(3, 2, 3)) |>
  mutate(LD = map2(LD, best_n, ~ slice_head(.x, n=.y))) |>
  select(set, LD) |>
  unnest(LD)
key_pcs <- reduced_pcs |> pull(PC) |> str_replace("_mean", "")

write_rds(comparison, here("models", "results", "comparison.rds"))
write_rds(reduced_pcs, here("models", "results", "reduced_pcs.rds"))
write_rds(key_pcs, here("models", "results", "key_pcs.rds"))