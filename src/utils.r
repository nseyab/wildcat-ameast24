# src/utils.r

#' Add expected goals against (xGA)
add_xGA <- function(data) {
  data |>
    group_by(Date) |>
    mutate(xGA = lead(xG)) |>
    ungroup()
}

#' abbreviate_Team
abbreviate_Team <- function(data) {
  data |>
    group_by(Date) |>
    mutate(
      abbTeam = case_when(
        str_starts(Team, "Albany") ~ "ALBY",
        str_starts(Team, "Binghamton") ~ "BING",
        str_starts(Team, "Bryant") ~ "BRY",
        str_starts(Team, "Maine") ~ "ME",
        str_starts(Team, "NJIT") ~ "NJIT",
        str_starts(Team, "UMBC") ~ "UMBC",
        str_starts(Team, "UMASS") ~ "UML",
        str_starts(Team, "New Hampshire") ~ "UNH",
        str_starts(Team, "Vermont") ~ "UVM"
      )) |>
    ungroup()  
}

#' add_Opp
add_Opp <- function(data) {
  data |>
    group_by(Date) |>
    mutate(Opp = lead(abbTeam)) |>
    ungroup()
}

#' get_data
get_data <- function(y, t) {
  if (t == "New-Hampshire" | t == "UMASS-Lowell") {
    t1 <- t
    t2 <- str_replace(t, "-", " ")
  }
  else {
    t1 <- t
    t2 <- t
  }
  atk <- read_excel(
      here("data", "raw", str_c(t1, "_Fall", y, "_Attacking.xlsx")), 
      skip=3, col_names=FALSE, .name_repair = "unique_quiet") |>
        setNames(ATK_COLS) |>
        filter(str_starts(Team, t2))
  def <- read_excel(
      here("data", "raw", str_c(t1, "_Fall", y, "_Defending.xlsx")), 
      skip=3, col_names=FALSE, .name_repair = "unique_quiet") |>
        setNames(DEF_COLS) |>
        filter(str_starts(Team, t2))
  gen <- read_excel(
      here("data", "raw", str_c(t1, "_Fall", y, "_General.xlsx")), 
      skip=3, col_names=FALSE, .name_repair = "unique_quiet") |>
        setNames(GEN_COLS) |>
        add_xGA() |>
        abbreviate_Team() |>
        add_Opp() |>
        filter(str_starts(Team, t2))
  idx <- read_excel(
      here("data", "raw", str_c(t1, "_Fall", y, "_Indexes.xlsx")), 
      skip=3, col_names=FALSE, .name_repair = "unique_quiet") |>
        setNames(IDX_COLS) |>
        filter(str_starts(Team, t2))
  pass <- read_excel(
      here("data", "raw", str_c(t1, "_Fall", y, "_Passing.xlsx")), 
      skip=3, col_names=FALSE, .name_repair = "unique_quiet") |>
        setNames(PASS_COLS) |>
        filter(str_starts(Team, t2))
  
  data <- left_join(gen, atk, by=join_by(Date, Match, Competition, Duration, Team, Scheme, xG, Shots, SoT, SoTP)) |>
    left_join(def, by=join_by(Date, Match, Competition, Duration, Team, Scheme)) |>
    left_join(idx, by=join_by(Date, Match, Competition, Duration, Team, Scheme)) |>
    left_join(pass, by=join_by(Date, Match, Competition, Duration, Team, Scheme, Passes, PA, PAP)) |>
    mutate(
      Date = date_parse(Date),
      Team = abbTeam,
      Points = case_when(
        Goals > Conceded_Goals ~ 3,
        Goals == Conceded_Goals ~ 1,
        Goals < Conceded_Goals ~ 0
      ),
      PResult = ifelse(Points == 0, FALSE, TRUE)) |>
    select(-c(Scheme, abbTeam))  

  return(data)
}

#' get_ic
get_ic <- function(data) {
  data |>
    filter(str_detect(Competition, "America East"))
}

#' get_ooc
get_ooc <- function(data) {
  data |>
    filter(str_detect(Competition, "Non-conference"))
}

#' get_games
get_games <- function(data) {
  data |>
    arrange(Date) |>
    mutate(
      Year = clock::date_format(Date, format="%y"),
      rown = 1:n(),
      ID = str_c(Team, Year, "-MD", rown)
    ) |>
    filter(!str_detect(Match, "\\([:upper:]\\)")) |>
    filter(!((Year == 24 & Date > "2024-11-02") |
              (Year == 23 & Date > "2023-10-29") |
              (Year == 22 & Date > "2022-10-29"))) |>
    select(ID, Team, Year, Date, Opp, Points, PResult, Goals:Goal_Kicks)
}

#' get_season
# get_season <- function(data) {
#   id <- data$ID[1] |> str_replace("-MD1", "")

#   data |> 
#     summarize(
#       Year = Year,
#       ID = id,
#       Team = Team,
#       Points = sum(Points),
#       AEWSOCC = factor(ifelse(ID %in% AEWSOCC, TRUE, FALSE)),
#       across(Goals:Goal_Kicks, ~ median(.x, na.rm=TRUE)),
#     )
# }
