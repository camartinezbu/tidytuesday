# TidyTuesday - Week 52
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(rtrek)

# Download and read datasets ----
# Sources
# Great British Bakeoff: Bakeoff package

tuesdata <- tidytuesdayR::tt_load(2022, week = 52)
tlBooks <- tuesdata$tlBooks

# Plot: Year ranges in the main television series

years_covered <- tlBooks |> 
  filter(setting == "primary") |> 
  group_by(series) |> 
  summarise(min_year = min(year), max_year = max(year)) |> 
  mutate(series = fct_rev(ordered(series, 
                                  levels = c("TOS", "TAS", "TNG",
                                             "DS9", "VOY", "ENT"),
                                  labels = c("The Original\nSeries",
                                             "The Animated\nSeries",
                                             "The Next\nGeneration",
                                             "Deep Space\nNine",
                                             "Voyager",
                                             "Enterprise")))) |> 
  drop_na()

ggplot(years_covered, aes(x = min_year, xend = max_year, y = series, yend = series)) +
  geom_segment(linewidth = 5, color = "#debb4e") +
  geom_label(aes(x=min_year-150, label = min_year), 
             color = "#ffffff", fill = "#040017", family = "Saira Condensed") +
  geom_label(aes(x=max_year+150, label = max_year), 
             color = "#ffffff", fill = "#040017", family = "Saira Condensed")+
  labs(
    # title = "Linea temporal de",
    title = "Timeline of",
    subtitle = "Star Trek",
    # caption = "\nFuente: Paquete {rtrek} | Elaborado por Camilo Martínez (@camartinezbu)\nNota: El gráfico incluye novelas, series y películas previas a Star Trek: Discovery.\nSe filtró el dataset para incluir únicamente el universo primario.",
    caption = "\nSource: {rtrek} library | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\nNote: The chart includes novels, television series and movies pre-Star Trek: Discovery.\nThe dataset was filtered to include only the primary universe.",
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    limits = c(1750, 4550), position = "top"
  ) +
  theme(
    text = element_text(color = "#ffffff"),
    plot.title = element_text(hjust = 0.5, family="Khan", size = 20),
    plot.subtitle = element_text(family = "Edge of the galaxy", size = 35, 
                                 color = "#debb4e", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Saira Condensed", hjust = 0.5),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#040017"),
    plot.margin = margin(t = 10,r = 50,b = 5,l = 50),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ffffff50"),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(color = "#ffffff", size = 12,
                             family = "Saira Condensed SemiBold"),
    axis.ticks.x = element_blank()
  )

# Export plot

ggsave("2022/2022-week52/plots/plot_w52_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")
