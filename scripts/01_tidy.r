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
    ic_games = map(ic, ~ get_games(.x))
  )

# Save tidy data
write_rds(ameast, here("data", "tidy", "ameast.rds"))

game <- ameast |> 
  pull(ic_games) |> 
  bind_rows() |>
  select(-any_of(DROP)) |>
  mutate(across(
    where(is.numeric) & ends_with("P") & !matches("AvgPP"),
    ~ .x / 100
  ))

season <- game |> 
  group_by(Team, Year) |> 
  summarize(
    Points = sum(Points),
    across(xG:Goal_Kicks, list(med = median, iqr = IQR)),
    .groups = "drop"
  ) |>
  mutate(ID = str_c(Team, Year), .before=Team) |>
  mutate(AEWSOCC = factor(ID %in% AEWSOCC), .after=Points)

write_rds(game, here("data", "tidy", "game.rds"))
write_rds(season, here("data", "tidy", "season.rds"))