# 01_tidy_data.r
# Combine Wyscout Excel files into a single team-year CSV
# Split team-year into "in conference" (IC) and "out of conference" (OOC)
# Finalize tidy data for use in PCA at game and season levels for in conference play

library(tidyverse)
library(readxl)
library(here)
library(clock)

source(here("src", "utils.r"))
source(here("src", "vars.r"))

ameast <- tibble(y = rep(YEARS, each=length(TEAMS)), t = rep(TEAMS, times=length(YEARS))) |>
  mutate(
    data = map2(y, t, ~ combine(.x, .y)),
    ic = map(data, ~ extract_ic(.x)),
    ooc = map(data, ~ extract_ooc(.x)),
    ic_data = map(ic, ~ tidy_ic(.x))
  )

ic_full <- ameast |> pull(ic_data) |> bind_rows() |> select(-c(Match, Competition, Duration))

ic_eff <- ic_full |>
  select(all_of(c(ID, RESPONSE, EFFICIENCY))) |>
  mutate(across(SoTP:TIAP, ~ .x / 100))

ic_vol <- ic_full |>
  select(all_of(c(ID, RESPONSE, VOLUME)))

ic_ctrl <- ic_full |>
  select(all_of(c(ID, RESPONSE, CONTROL)))

# Save tidy data
write_rds(ameast, here("data", "tidy", "ameast.rds"))
write_rds(ic_full, here("data", "tidy", "ic_full.rds"))
write_rds(ic_eff, here("data", "tidy", "ic_eff.rds"))
write_rds(ic_vol, here("data", "tidy", "ic_vol.rds"))
write_rds(ic_ctrl, here("data", "tidy", "ic_ctrl.rds"))