# TidyTuesday - Week 39
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(usmapdata)
library(patchwork)

# Download and read datasets ----
# Sources
# Artists: https://www.arts.gov/impact/research/arts-data-profile-series/adp-31/data-tables
# US abbreviated names: https://worldpopulationreview.com/states/state-abbreviations

names <- read_csv("2022/2022-week39/data/abbr_names.csv")

artists <- read_csv("2022/2022-week39/data/artists.csv")

# Plot 1: Tiles - Distribution of actors according to race

actor_race_share_by_state <- artists |> 
  filter(state != "Puerto Rico", type == "Actors") |> 
  group_by(state, type) |> 
  mutate(race_share = artists_n/sum(artists_n, na.rm = TRUE)) |> 
  ungroup() |> 
  left_join(names, by = c("state" = "State")) |> 
  replace_na(list(race_share = 0))

plot_1 <- ggplot(actor_race_share_by_state , aes(x = Code, y = race, fill = race_share)) +
  geom_tile(width = 1, height = 1, color = "#000000", lwd = 0.11) +
  labs(
    y = NULL,
    x = NULL,
    fill = "% de actores\ndesagregados\nsegún raza",
    title = "Distribución de actores en Estados Unidos",
    subtitle = "La mayoría de actores en Estados Unidos son Blancos y se encuentran\nen mayor proporción en los estados de California y Nueva York."
  ) +
  coord_fixed(ratio = 1) + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = c("Other", "African-American", "Asian",  "Hispanic", "White"),
                   labels = c("Otros", "Afroamericanos", "Asiáticos", "Hispanos", "Blancos")) + 
  scale_fill_gradientn(colors = c(
    "#FFFFFF", # White
    "#BB80CB", # African Violet
    "#760096" # Violet RYB
  ),
  labels = scales::label_percent()) +
  theme(
    text = element_text(family = "Kanit"),
    plot.title = element_text(family = "Kanit SemiBold", size = 22, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8.5),
    legend.text = element_text(size = 7),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 5.5),
    axis.text.y = element_text(size = 8)
  )

# Plot 2: Map - Share of actors over total workers 

actor_total_by_state <- artists |> 
  filter(state != "Puerto Rico", type == "Actors") |>
  group_by(state) |> 
  summarise(actor_share = sum(artists_n, na.rm = TRUE)/sum(all_workers_n, na.rm = TRUE)) |> 
  ungroup() |> 
  left_join(names, by = c("state" = "State"))

plot_2 <- usmap::plot_usmap(data = actor_total_by_state, values = "actor_share") +
  labs(
    fill = "% de actores\nsobre el total\nde trabajadores",
    caption = "Fuente: arts.gov | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_fill_gradientn(colors = c(
    "#FFFFFF", # White
    "#BB80CB", # African Violet
    "#760096" # Violet RYB
  ),
  labels = scales::label_percent()
  ) +
  theme(
    text = element_text(family = "Kanit"),
    legend.position = c(-0.14, 0.3),
    legend.title = element_text(size = 8.5),
    legend.text = element_text(size = 7)
  )

# Full plot using patchwork

full_plot <-  plot_1 / plot_2 +
  plot_layout(heights = c(1, 7))

ggsave("2022/2022-week39/plots/plot_w39.png", 
       width = 2100, 
       height = 2000, 
       units = "px")
