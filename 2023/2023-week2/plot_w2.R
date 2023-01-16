# TidyTuesday - Week 2
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggsvg)

# Download and read datasets ----
# Sources
# Bird FeederWatchData: https://feederwatch.org/explore/raw-dataset-requests/

pfw_2021 <- tidytuesdayR::tt_load(2023, week = 02)$PFW_2021_public

most_common_species_us <- pfw_2021 |> 
  filter(valid == 1, str_detect(subnational1_code, "US")) |> 
  count(species_code, wt = how_many) |> 
  arrange(desc(n))

# Plot: Isotype of Dark Eyed Junco Sightings

grid <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = 21,
                          ncol = 12)) |> 
  mutate(row = row_number()) |> 
  pivot_longer(starts_with("X"), names_to = "column") |> 
  mutate(column = as.integer(str_replace(column, "X", ""))) |>
  select(-value) |> 
  filter(!(row == 1 & column == 12))

ggplot(grid) +
  geom_point_svg(aes(x = column*1.25, y = row), svg = paste(readLines("~/Downloads/test.svg"), collapse = "\n")) +
  geom_point_svg(x = 5*1.25, y = 23.3, svg = paste(readLines("~/Downloads/test.svg"), collapse = "\n")) +
  annotate("text", x = 7*1.25, y = 23.3, label = "= 100 sightings", family = "Jost Medium") +
  labs(
    title = "El junco pizarroso fue el ave más avistada en\nEstados Unidos durante la temporada 2021*",
    subtitle = "En total se registraron 25.093 avistamientos en este periodo",
    caption = "Fuente: feederwatch.org | Elaborado por Camilo Martínez (@camartinezbu)\n*La temporada 2021 corresponde al periodo comprendido entre noviembre de 2020 y abril de 2021",
    x = NULL,
    y = NULL
  ) +
  # labs(
  #   title = "The dark eyed junco was the most observed bird\nin the United States during the 2021 season*",
  #   subtitle = "25.093 individuals were observed in this period",
  #   caption = "Source: feederwatch.org | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\n*The 2021 season begins on november 2020 and ends on april 2021",
  #   x = NULL,
  #   y = NULL
  # ) +
  coord_cartesian(xlim = c(0, 16.5), ylim = c(0, 25), expand = FALSE) +
  theme(
    text = element_text(family = "Jost Medium"),
    plot.title = element_text(hjust = 0.5, family = "Jost Black", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 8),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#fbd18e", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )
  
# Export plot

ggsave("2023/2023-week2/plots/plot_w2.png", 
       width = 1800, 
       height = 1600, 
       units = "px")