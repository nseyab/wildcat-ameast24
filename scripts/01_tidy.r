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
    data = map2(y, t, ~ get_data(.x, .y)),
    ic = map(data, ~ get_ic(.x)),
    ooc = map(data, ~ get_ooc(.x)),
    games = map(ic, ~ get_games(.x)),
    season = map(games, ~ get_season(.x))
  )

# Save tidy data
write_rds(ameast, here("data", "tidy", "ameast.rds"))

# # Game-level Data
# aeconf |> 
#   pull(games) |>
#   bind_rows() |>
#   write_rds(here("data", "tidy", "games.rds"))

# # Season-level Data
# aeconf |>
#   pull(season) |>
#   bind_rows() |>
#   write_rds(here("data", "tidy", "season.rds"))