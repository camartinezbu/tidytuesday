# TidyTuesday - Week 45
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(readxl)
library(sf)
library(cowplot)

# Download and read datasets ----
# Sources
# State Stations: Wikipedia
# State Abbreviations: https://worldpopulationreview.com/states/state-abbreviations
# Cities: https://geodata.lib.berkeley.edu/catalog/stanford-bx729wr3020

data <- tidytuesdayR::tt_load(2022, week = 45)

cities <- sf::st_read("2022/2022-week45/data/geometry/cities/bx729wr3020.shp")

abbr <- read_csv("2022/2022-week45/data/abbreviations/abbreviations.csv")

npr <- read_xlsx("2022/2022-week45/data/NPR/NPR.xlsx")

stations <- data$state_stations |> 
  mutate(state = str_replace(state, "_", " ")) |> 
  left_join(abbr, by = "state") |> 
  select(call_sign, city, code) |> 
  left_join(select(npr, call_sign, service, npr), by = "call_sign")

geo_stations <- cities |> 
  right_join(stations, by = c("name" = "city", "state" = "code")) |> 
  filter(npr == 1)

# Plot: A map of the US based on public radio stations

plot <- ggplot(geo_stations) +
  geom_sf(aes(color = service), alpha = 1, size = 0.001) +
  labs(
    title = "Un mapa de Estados Unidos\na partir de estaciones de radio de la \n\n",
    caption = "\nFuente: Wikipedia | Elaborado por Camilo Martínez (@camartinezbu)",
    color = NULL
  ) +
  scale_color_manual(
    values = c("#C5362d", "#3f79b8")
  ) +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    text = element_text(color = "white"),
    plot.title = element_text(family = "Raleway ExtraBold", hjust = 0.5, size = 15),
    plot.caption = element_text(family = "Raleway", hjust = 0.5, size = 8),
    plot.background = element_rect(fill = "#101010", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = c(0.85,0.95),
    legend.justification = "center",
    legend.text = element_text(family = "Raleway", size = 7),
    legend.key.size = unit(4, "mm"),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

logo <- "2022/2022-week45/data/NPR/logo.png"

ggdraw() +
  draw_plot(plot) +
  draw_image(logo, scale = 0.2, y = 0.285)
  
# Export plot

ggsave("2022/2022-week45/plots/plot_w45.png", 
       width =1500, 
       height = 1500, 
       units = "px")
