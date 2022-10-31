# TidyTuesday - Week 42
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggfx)
library(cowplot)
library(jpeg)

# Download and read datasets ----
# Sources
# Episodes and dialogue: https://8flix.com/collections/transcripts/stranger-things-2/
# Stranger things color palette: http://www.katharinefriedgen.com/design-in-the-everyday-title-sequences/
episodes <- read_csv("2022/2022-week42/data/episodes.csv")
dialogue <- read_csv("2022/2022-week42/data/stranger_things_all_dialogue.csv")

# Plot: Count of grunts in all episodes

grunt_count <- dialogue |>
  mutate(grunt = str_detect(stage_direction, "grunt")) |>
  filter(grunt) |>
  group_by(season, episode) |>
  summarise(grunt_total = n()) |> 
  ungroup() |> 
  mutate(episode_count = row_number())

plot <- ggplot(grunt_count, aes(x = episode_count, y = 1, fill = grunt_total)) +
  with_outer_glow(geom_tile(), colour = "#DD635B", sigma = 15) +
  labs(
    title = "\n\n\n\nNumero de gruñidos en el guión",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "\nFuente: 8flix.com | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_x_continuous(
    breaks = c(1, 9, 18, 26, 34)-0.25,
    labels = c("T1-E1", "T2-E1", "T3-E1", "T4-E1", "T4-E9")
  ) +
  scale_fill_gradientn(colours = c(
    "#0c0000",
    "#3f070a",
    "#59070b",
    "#b92b22"
  )) +
  theme_minimal() +
  theme(
    text = element_text(colour = "#bbbbbb", family = "IBM Plex Mono"),
    plot.title = element_text(family = "IBM Plex Mono Medium",
                              size = 12, hjust = 0.5),
    plot.caption = element_text(size = 6, hjust = 0.5),
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "IBM Plex Mono Medium",
                               colour = "#bbbbbb", size = 7, angle = 90),
    legend.key.height = unit(4, "mm"),
    legend.text = element_text(size = 6),
    legend.margin = margin(t = 5, r = 0, b = -5, l = 0),
    legend.position = "top",
    legend.direction = "horizontal"
  )

logo <- "2022/2022-week42/data/logo.jpg"

ggdraw() +
  draw_plot(plot) +
  draw_image(logo, scale = .35, y = 0.4)

# Export plot

ggsave("2022/2022-week42/plots/plot_w42.png",
       width = 1500,
       height = 1200,
       unit = "px")
