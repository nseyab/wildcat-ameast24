# 03_umap.r
# Plots for UMAP at game and season levels

library(tidyverse)
library(here)
library(ggrepel)

#------------------------
# Prep data for plotting
#------------------------
umap_rec <- read_rds(here("output", "umap_rec.rds"))

umap <- juice(umap_rec) |>
  mutate(
    Result = factor(
      case_when(
        Points == 3 ~ "Win",
        Points == 1 ~ "Draw",
        .default = "Loss"
      ), 
      levels=c("Win", "Draw", "Loss")
    ),
    Anno = str_c(Team, "-", Opp, "-", Year)
  ) 

lims <- umap |> 
  select(UMAP1, UMAP2) |>  
  summarise(
    min_x = floor(min(UMAP1)),
    max_x = ceiling(max(UMAP1)),
    min_y = floor(min(UMAP2)),
    max_y = ceiling(max(UMAP2))
  )

umap <- umap |>
  group_by(Team, Year) |>
  nest() |>
  mutate(
    Anno = str_c(Team, Year),
    UMAP1_s = map_dbl(data, ~ mean(.x$UMAP1)),
    UMAP2_s = map_dbl(data, ~ mean(.x$UMAP2)),
    AEWSOCC = factor(ifelse(Anno %in% AEWSOCC, TRUE, FALSE), levels=c("TRUE", "FALSE"))
  )

#------------------------------
# Conference Teams All Seasons
#------------------------------
fig_umap <- umap |>
  ggplot(aes(x=UMAP1_s, y=UMAP2_s, shape=AEWSOCC, fill=Team, color=Team, alpha=Year)) +
  geom_point(size=4, stroke=0.7) +
  geom_text_repel(aes(label=Anno, color=Team), size=11, max.overlaps = 20, box.padding = 0.4, segment.color = NA) +
  scale_x_continuous(limits=c(lims$min_x, lims$max_x)) +
  scale_y_continuous(limits=c(lims$min_y, lims$max_y)) +
  coord_fixed() +
  scale_alpha_manual(values = c(0.4, 0.4, 1)) +
  scale_shape_manual(values = c(24, 25)) +
  scale_fill_manual(values = PAL_SECONDARY) +
  scale_color_manual(values = PAL_PRIMARY) +
  labs(
    x=NULL, y=NULL, 
    title = "Uniform Manifold Approximation & Projection (UMAP) by Season",
    caption = "&#9651; America East Conference Championship Qualification"
  ) +
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.caption = ggtext::element_markdown()
  )

ggsave(here("figs", "fig_umap.png"), fig_umap, dpi="retina")

#--------------------
# UNH - 2024 Season
#--------------------
umap_unh24 <- umap |>
  filter(Anno == "UNH24") |>
  select(-c(Anno, AEWSOCC)) |> 
  unnest(data) |>
  mutate(
    Result = factor(
      case_when(
        Points == 3 ~ "Win",
        Points == 1 ~ "Draw",
        .default = "Loss"
      ), 
      levels=c("Win", "Draw", "Loss")
    ),
    Anno = str_c(str_sub(ID, -3), "-", Opp)
  )

unh24_x <- umap_unh24$UMAP1_s[1]
unh24_y <- umap_unh24$UMAP2_s[1]

fig_umap_unh24 <- umap_unh24 |>
  ggplot(aes(x=UMAP1, y=UMAP2, shape=Result, fill=Opp, color=Opp)) +
  geom_point(size=4, stroke=0.7) +
  geom_text_repel(aes(label=Anno, color=Opp), size=11, max.overlaps = 14, box.padding = 0.6, segment.color = NA) +
  annotate("point", x=unh24_x, y=unh24_y, size=4, stroke=0.7, shape=8, color="#003591") +
  annotate("text", x=unh24_x+0.9, y=unh24_y, label="Season Center", size=11, color="#003591") +
  scale_x_continuous(limits=c(lims$min_x, lims$max_x)) +
  scale_y_continuous(limits=c(lims$min_y, lims$max_y)) +
  coord_fixed() +
  scale_shape_manual(values = c("Win"=24, "Draw"=23, "Loss"=25)) +
  scale_fill_manual(values = PAL_SECONDARY) +
  scale_color_manual(values = PAL_PRIMARY) +
  labs(
    x=NULL, y=NULL, 
    title = "UNH 2024 Season UMAP by Game",
    caption = "&#9651; Win  &#9661; Loss"
  ) +
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.caption = ggtext::element_markdown()
  )

ggsave(here("figs", "fig_umap_unh24.png"), fig_umap_unh24, dpi="retina")