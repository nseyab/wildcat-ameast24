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

#' combine
combine <- function(y, t) {
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
      PResult = ifelse(Points == 0, FALSE, TRUE),
      Result = case_when(
        Points == 3 ~ "W",
        Points == 1 ~ "D",
        Points == 0 ~ "L"
      ),
      FwdPP = Forward_Passes / Passes,
      BackPP = Back_Passes / Passes,
      LatPP = Lateral_Passes / Passes,
      PtFTPP = Passes_to_Final_Third / Passes,
      ProgPP = Progressive_Passes / Passes,
      SmPP = Smart_Passes / Passes,
      AvgSxG = xG / Shots,
      AvgSAxG = xGA / Shots_Against 
    ) |>
    select(-c(Scheme, abbTeam))  

  return(data)
}

#' extract_ic
extract_ic <- function(data) {
  data |>
    filter(str_detect(Competition, "America East"))
}

#' extract_ooc
extract_ooc <- function(data) {
  data |>
    filter(str_detect(Competition, "Non-conference"))
}

#' tidy_ic
tidy_ic <- function(data) {
  data |>
    arrange(Date) |>
    mutate(
      Year = clock::date_format(Date, format="%y"),
      row_n = 1:n(),
      ID = str_c(Team, Year, "-MD", row_n)
    ) |>
    # exclude non-regular season games
    filter(!str_detect(Match, "\\([:upper:]\\)")) |>
    filter(!((Year == 24 & Date > "2024-11-02") |
              (Year == 23 & Date > "2023-10-29") |
              (Year == 22 & Date > "2022-10-29"))) |>
    select(-row_n)
}

#' create_umap_rec
create_umap_rec <- function(data) {
  recipe(~., data=data) |>
    update_role(ID, Team, Year, Date, Opp, new_role="id") |>
    update_role(PResult, new_role="outcome") |>
    step_YeoJohnson(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_umap(all_numeric_predictors(), neighbors=5, min_dist=0.4)
}
# , neighbors=7, min_dist=0.14


#' create_umap_plot
plot_umap <- function(split, rec) {

  data <- rec |>
    prep() |>
    bake(new_data=NULL) |>
    mutate(
      PResult = factor(PResult, levels=c("TRUE", "FALSE")),
      Anno = str_c(Team, "-", Opp, "-", Year)
    ) 

  sum_data <- data |>
    group_by(Team, Year) |>
    nest() |>
    mutate(
      Anno = str_c(Team, Year),
      UMAP1_s = map_dbl(data, ~ mean(.x$UMAP1)),
      UMAP2_s = map_dbl(data, ~ mean(.x$UMAP2)),
      AEWSOCC = factor(ifelse(Anno %in% AEWSOCC, TRUE, FALSE), levels=c("TRUE", "FALSE"))
    )

  # Only show 2024 teams + all UNH years for feature-specific plots
  if(split != "full"){
    sum_data <- sum_data |> filter(str_detect(Anno, "UNH|24"))
  }

  p <- sum_data |>
    ggplot(aes(x=UMAP1_s, y=UMAP2_s, shape=AEWSOCC, fill=Team, color=Team, alpha=Year)) +
    geom_point(size=5, stroke=0.7) +
    geom_text_repel(aes(label=Anno, color=Team), size=20, max.overlaps = 20, box.padding = 0.6, segment.color = NA) +
    scale_alpha_manual(values = c(0.4, 0.4, 1)) +
    scale_shape_manual(values = c(24, 25)) +
    scale_fill_manual(values = PAL_SECONDARY) +
    scale_color_manual(values = PAL_PRIMARY) +
    labs(x=NULL, y=NULL) +
    theme(
      legend.position = "none", 
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
    )
}

#' create_pca_rec
create_pca_rec <- function(data) {
  recipe(~., data=data) |>
    update_role(ID, Team, Year, Date, Opp, new_role="id") |>
    update_role(PResult, new_role="outcome") |>
    step_YeoJohnson(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_pca(all_numeric_predictors(), threshold=0.8)
}

#' create_cpve_plot
plot_cpve <- function(rec) {

  data <- rec |>
    prep()
  
  p <- data |>
    tidy(3, type="variance") |>
    filter(str_detect(terms, "cumulative percent")) |>
    ggplot(aes(component, value)) +
    geom_point(color="#001D52") +
    geom_line(color="#001D52") +
    geom_hline(yintercept=80, linetype="dashed", color="#CB4D0B") +
    annotate("text", x=20, y=81, label="80% Threshold", color="#CB4D0B")
}

#' create_class_rec
create_class_rec <- function(data, model) {
  data <- data |> mutate(across(any_of(c("ID", "Team", "Year", "AEWSOCC")), as.factor))

  recipe(AEWSOCC ~., data=data) |>
    update_role(ID, Team, Year, new_role="id")

}

#' extract_fit
extract_fit <- function(spec, rec) {
  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(spec)

  data <- rec |> prep() |> bake(new_data=NULL)

  if (spec$engine!= "MASS") {
    
    resamples <- bootstraps(data, times=1000, strata="AEWSOCC")

    tuned <- wf |>
      tune::tune_grid(
        resamples = resamples,
        grid = 10,
        metrics = metric_set(accuracy)
      )

    best_params <- tuned |> tune::select_best(metric="accuracy")

    finalized <- wf |> tune::finalize_workflow(best_params)

    finalized |> fit(data=data)

  } else {
    wf |> fit(data=data)
  }
}

#' add_preds(fit, data, response)
add_preds <- function(data, model, fit) {
  response <- ifelse(model == "mnom_reg", "Table", "AEWSOCC")
  
  data |>
    mutate(
      .pred = predict(fit, new_data=data)$.pred_class,
      .truth = .data[[response]]
    )
}

#' extract_pca_loadings(s_rec, top_n = 6)
extract_loadings_value <- function(rec, tau=0.2) {
  rec |>
    prep() |>
    tidy(3, type="coef") |>
    group_by(component) |>
    arrange(desc(abs(value)), .by_group = TRUE) |>
    mutate(sign = ifelse(value > 0, "+", "-")) |>
    # filter(round(abs(value), 3) >= tau) |>
    ungroup()
}

#' prep_for_plots(set, data)
prep_for_plots <- function(set, data) {
  key_pcs <- read_rds(here("models", "results", "key_pcs.rds"))
  i <- str_which(key_pcs, set)
  pcs <- key_pcs[i]
  pcs <- c(str_c(pcs, "_mean"), str_c(pcs, "_sd"))

  data |> select(any_of(c("ID", "Team", "Year", "AEWSOCC", pcs)))
}

#' plot_key_pcs(labs, data)
plot_key_pcs <- function(labs, data) {
  # Generic pattern to match any alphabetic prefix followed by PC info and either mean or sd.
  pattern <- "^[[:alpha:]]+_(PC[[:digit:]]+)_(mean|sd)$"
  
  data <- data |>
    filter(Year == "24") |>
    pivot_longer(
      cols = contains("PC"),
      names_to = c("PC", ".value"),
      names_pattern = pattern
    ) |>
    group_by(PC) |>
    mutate(mean_scaled = scale(mean)) |>
    ungroup() |>
    mutate(PC = factor(PC, levels = labs$PC))

  facet_labeller <- as_labeller(setNames(str_wrap(labs$title, width=20), labs$PC))
  
  p <- data |>
    ggplot(aes(x = mean_scaled, y = factor(Team), color = Team, fill = Team, shape = AEWSOCC)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#DCDEDF") +
    geom_errorbarh(aes(xmin = mean_scaled - sd, xmax = mean_scaled + sd), height = 0.6, linewidth = 0.7, alpha = 0.4) +
    geom_point(size = 3, stroke = 0.7) +
    facet_wrap(~ PC, ncol = 1, strip.position = "left", labeller = facet_labeller) +
    scale_y_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    scale_shape_manual(values = c(24, 25)) +
    scale_fill_manual(values = PAL_SECONDARY) +
    scale_color_manual(values = PAL_PRIMARY) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.line.x = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle=90, size=32, lineheight=0.4)
    )
}

#' plot_pcs_xy(labs, data)
plot_pcs_xy <- function(labs, data) {
  # Generic pattern to match any alphabetic prefix followed by PC info and either mean or sd.
  pattern <- "^[[:alpha:]]+_(PC[[:digit:]]+)_(mean|sd)$"
  i <- str_which(names(data), pattern)[2]
  
  lim <- data |>
    select(any_of(c(i, i-1))) |>
    summarize(lim = max(
      max(c_across(everything()), na.rm = TRUE), abs(min(c_across(everything()), na.rm = TRUE))
    )) |>
    pull(lim)

  xcol <- names(data)[i-1]
  ycol <- names(data)[i]

  xlab <- labs$title[1]
  ylab <- labs$title[2]

  offset_x <- 0.05 * lim

  p <- data |>
    filter(str_detect(ID, "UNH|24")) |>
    mutate(Anno = ifelse(Team == "UNH" & Year != 24, as.character(Year), NA)) |>
    ggplot(aes(x = .data[[xcol]], y = .data[[ycol]], color = Team, fill = Team, shape = AEWSOCC, alpha = Year)) +

    geom_vline(xintercept = 0, linetype = "dashed", color = "#DCDEDF") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#DCDEDF") +

    geom_point(size = 5, stroke = 0.7) +
    geom_text_repel(aes(label=Anno, color=Team), size=18, max.overlaps = 20, box.padding = 0.6, segment.color = NA) +

    # x-axis annotations: low (left) and high (right)
    annotate("text", x = -lim, y = offset_x, label = str_wrap(labs$low[1], width = 20), 
             hjust = 0, vjust = 0, angle = 0, size = 8, lineheight=0.4) +
    annotate("text", x = lim, y = offset_x, label = str_wrap(labs$high[1], width = 20), 
             hjust = 1, vjust = 0, angle = 0, size = 8, lineheight=0.4) +
    # y-axis annotations: low (bottom) and high (top) at x = 0
    annotate("text", x = 0, y = -lim, label = str_wrap(labs$low[2], width = 32), 
             hjust = 0.5, vjust = 0.5, angle = 0, size = 8, lineheight=0.4) +
    annotate("text", x = 0, y = lim, label = str_wrap(labs$high[2], width = 40), 
             hjust = 0.5, vjust = 0.5, angle = 0, size = 8, lineheight=0.4) +
    
    scale_x_continuous(limits = c(-lim, lim), expand = expansion(mult = c(0.05, 0.05))) +
    scale_y_continuous(limits = c(-lim, lim), expand = expansion(mult = c(0.05, 0.05))) +
    coord_fixed() +
    scale_shape_manual(values = c(24, 25)) +
    scale_alpha_manual(values = c(0.4, 0.4, 1)) +
    scale_fill_manual(values = PAL_SECONDARY) +
    scale_color_manual(values = PAL_PRIMARY) +
    labs(x=xlab, y=ylab) +
    theme(
      legend.position     = "none",
      plot.caption        = ggtext::element_markdown(),
      panel.grid.major.y  = element_blank()
    )
}