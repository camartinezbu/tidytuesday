# TidyTuesday - Week 7
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)

# Download and read datasets ----
# Sources
# Holywood Age Gap: https://hollywoodagegap.com/

ages <- tidytuesdayR::tt_load(2023, week = 7)$age_gaps

# Plot: Difference between Actors ages ----

ages_clean <- ages |> 
  mutate(
    decade = as.numeric(paste0(str_sub(release_year, 1, 3), "0"))
  ) |> 
  group_by(decade) |> 
  summarise(across(c(actor_1_age, actor_2_age),
                   .fns = list(mean = mean, min = min, max = max, median = median),
                   .names = "{.fn}_{.col}"
  )) |> 
  mutate(
    mean_age_diff = mean_actor_1_age - mean_actor_2_age,
    diff_pos = (mean_actor_1_age + mean_actor_2_age)/2
  ) |> 
  ungroup()

COLOR_ACTOR_1 <- "#279AF1"
COLOR_ACTOR_2 <- "#EA526F"
SIZE_SHAPE <- 3
SIZE_TEXT_LEGEND <- 2
FONT_FAMILY <- "Lexend"

ggplot(ages_clean) +
  geom_rect(xmin = 2020, xmax = 2050, ymin = 19, ymax = 81, fill = "#FDF8D8", color = NA) +
  geom_segment(aes(x = decade, xend = decade,
                   y = mean_actor_2_age, yend = mean_actor_1_age)) +
  geom_ribbon(aes(x = decade, y = max_actor_1_age,
                  ymin = mean_actor_2_age, ymax = mean_actor_1_age,
                  xmin = 1930, xmax = 2020), fill = "#10101010") +
  geom_ribbon(aes(x = decade, y = max_actor_1_age,
                  ymin = min_actor_2_age, ymax = max_actor_1_age,
                  xmin = 1930, xmax = 2020), fill = "#05050510") +
  geom_line(aes(x = decade, y = max_actor_1_age, group = NA), color = paste0(COLOR_ACTOR_1, "50")) +
  geom_point(aes(x = decade, y = mean_actor_1_age), shape = "circle filled",
             fill = COLOR_ACTOR_1, color = "white", size = SIZE_SHAPE) +
  geom_label(aes(x = decade, y = diff_pos, label = round(mean_age_diff)), 
             fill = "#FDF8D8", label.size = NA, fontface = "bold", size = 2.75) +
  geom_point(aes(x = decade, y = mean_actor_2_age), shape = "square filled",
             fill = COLOR_ACTOR_2, color = "white", size = SIZE_SHAPE) +
  geom_line(aes(x = decade, y = min_actor_2_age, group = NA), color = paste0(COLOR_ACTOR_2, "50")) +
  geom_text(aes(x = 2024, y = 71, label = "Edad de\nla persona\nmás mayor"),
            size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
            color = paste0(COLOR_ACTOR_1, "30")) +
  geom_text(aes(x = 2024, y = 50, label = "Edad promedio de\nla persona mayor\nen la relación"),
            size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
            color = COLOR_ACTOR_1) +
  geom_text(aes(x = 2024, y = 42.44, label = "Diferencia de\nedades promedio"),
            size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY) +
  geom_text(aes(x = 2024, y = 34, label = "Edad promedio de\nla persona menor\nen la relación"),
            size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
            color = COLOR_ACTOR_2) +
  geom_text(aes(x = 2024, y = 24, label = "Edad de\nla persona\nmás joven"),
            size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
            color = paste0(COLOR_ACTOR_2, "30")) +
  labs(
    title = "¿Si tuviera 30?",
    subtitle = "La edad promedio de las personas que actúan como intereses románticos\nen películas de Hollywood ha aumentado desde los 90s",
    x = "Década",
    y = "Años",
    caption = "Fuente: Hollywood Age Gap via Data is Plural | Elaborado por Camilo Martínez (@camartinezbu)\nNota: El gráfico describe una muestra de 1.155 películas. La más reciente es de 2022."
  ) +
  # geom_text(aes(x = 2024, y = 71, label = "Age of\nthe oldest\nperson"),
  #         size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
  #         color = paste0(COLOR_ACTOR_1, "30")) +
  # geom_text(aes(x = 2024, y = 50, label = "Avg. age of the\nolder person\nin the relationship"),
  #           size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
  #           color = COLOR_ACTOR_1) +
  # geom_text(aes(x = 2024, y = 42.44, label = "Difference between\navg. ages"),
  #           size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY) +
  # geom_text(aes(x = 2024, y = 34, label = "Avg. age of the\nyounger person\nin the relationship"),
  #           size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
  #           color = COLOR_ACTOR_2) +
  # geom_text(aes(x = 2024, y = 24, label = "Age of\nthe youngest\nperson"),
  #           size = SIZE_TEXT_LEGEND, hjust = 0, family = FONT_FAMILY,
  #           color = paste0(COLOR_ACTOR_2, "30")) +
  # labs(
  #   title = "¿13 Going on 30?",
  #   subtitle = "Tha average age of the people that act as romantic interests\nin Hollywood movies has increased since the 90s",
  #   x = "Decade",
  #   y = "Years",
  #   caption = "Source: Hollywood Age Gap via Data is Plural | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\nNote: The plot describes a sample of 1.155 movies. The most recent movie was released in 2022."
  # ) +
  scale_x_continuous(limits = c(1930, 2030), breaks = seq(1930, 2020, 10),
                     expand = expansion(add = c(5, 12.5))) +
  scale_y_continuous(limits = c(15, 87), breaks = seq(20, 80, 10)) +
  theme(
    text = element_text(family = "Lexend"),
    plot.title = element_text(size = 25, family = "Righteous", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 8, margin = margin(t = 15, unit = "pt")),
    plot.background = element_rect(fill = "#FDF8D8", color = NA),
    plot.margin = margin(t = 15, r = 20, b = 5, l = 20, unit = "pt"),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#50505020"),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10, "pt"))
  )

# Export plot

ggsave("2023/2023-week7/plots/plot_7.png", 
       width = 2000, 
       height = 1800, 
       units = "px")
    
    
    