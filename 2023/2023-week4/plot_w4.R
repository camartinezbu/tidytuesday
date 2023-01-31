# TidyTuesday - Week 4
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggimage)
library(ggtext)

# Download and read datasets ----
# Sources
# Alone: https://github.com/doehm/alone

alone <- tidytuesdayR::tt_load(2023, week = 04)

survivalists_season_1 <- alone$survivalists |> 
  filter(season == 1) |> 
  select(name, result, days_lasted, reason_tapped_out) |> 
  bind_cols(image = paste0("./2023/2023-week4/images/", c("flag.png",
                                             "brain.png",
                                             "heart.png",
                                             "sun.png",
                                             "bolt.png",
                                             "blood-drop.png",
                                             "paw.png",
                                             "wrench.png",
                                             "paw.png",
                                             "paw.png"))) |> 
  bind_cols(y_off = c(-0.5, 1.5, 0.75, -1.5, 0.5, -0.5, 1, 1.33, -1, 1.85))

# Plot: timeline of season 1.

ggplot(survivalists_season_1) +
  geom_segment(x = 0, xend = 56, y = 0, yend = 0, color = "#454955", linewidth = 1) +
  geom_segment(aes(x = days_lasted, xend = days_lasted, y = y_off, yend = 0, )) +
  geom_point(aes(x = days_lasted, y = 0), 
             size = 3.5, color = "#454955",
             shape = 21, fill = "#72B01D") +
  geom_point(aes(x = days_lasted, y = y_off),
             shape = 21, fill = "#F3EFF5", color = "#72B01D",
             size = 6, stroke = 2) +
  geom_image(aes(x = days_lasted, y = y_off, image = image), size = 0.02) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 1:</strong><br>Josh Chavez se retira <br>por miedo a los osos.",
    x = 2, y = 1.85, fill = NA, label.color = NA,
    hjust = 0, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 2:</strong><br>Chris Weatherman se retira <br>por miedo a los lobos.",
    x = 3, y = -1, fill = NA, label.color = NA,
    hjust = 0, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 5:</strong><br>Joe Robinet se retira <br>por perder su pedernal...",
    x = 6, y = 1.33, fill = NA, label.color = NA,
    hjust = 0, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "y Wayne Russell por su <br>por miedo a los osos.",
    x = 6, y = 1, fill = NA, label.color = NA,
    hjust = 0, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 7:</strong><br>Brant McGee se retira <br>consumir agua salada.",
    x = 8, y = -0.5, fill = NA, label.color = NA,
    hjust = 0, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 9:</strong><br>Dustin Feher se retira <br>por miedo a los tormetas.",
    x = 10, y = 0.5, fill = NA, label.color = NA,
    hjust = 0, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 40:</strong><br>Lucas Miller se retira por<br>estar satisfecho con<br>su desempeño.",
    x = 37, y = -1.5, fill = NA, label.color = NA,
    hjust = 1, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 44:</strong><br>Mitch Mitchell se retira<br>para estar con su mamá<br>durante su cancer.",
    x = 41, y = 0.75, fill = NA, label.color = NA,
    hjust = 1, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 56:</strong><br>Sam Larson pierde<br>el juego mental.",
    x = 53, y = 1.5, fill = NA, label.color = NA,
    hjust = 1, size = 2.5, family = "Unbounded"
  ) +
  geom_richtext(
    label = "<strong style='color:#72B01D'>Día 57:</strong><br>Alan Kay es el<br>ganador de Alone.",
    x = 54, y = -0.5, fill = NA, label.color = NA,
    hjust = 1, size = 2.5, family = "Unbounded"
  ) +
  labs(
    title = "Línea de tiempo de la\nprimera temporada de Alone",
    subtitle = "Alone es un programa de History en el que 10 personas compiten\npara sobrevivir el mayor tiempo posible en entornos remotos",
    caption = "Fuente: Alone data package | Elaborado por Camilo Martínez (@camartinezbu)\nLas imágenes usadas se descargaron de flaticon.com",
    y = NULL,
    x = NULL,
  ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 1:</strong><br>Josh Chavez retires<br>for fear of bears.", 
  #   x = 2, y = 1.85, fill = NA, label.color = NA,
  #   hjust = 0, size = 2.5, family = "Unbounded"
  # ) + 
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 2:</strong><br>Chris Weatherman retires<br>for fear of wolves.", 
  #   x = 3, y = -1, fill = NA, label.color = NA,
  #   hjust = 0, size = 2.5, family = "Unbounded"
  # ) + 
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 5:</strong><br>Joe Robinet retires<br>for losing his ferro rod...",
  #   x = 6, y = 1.33, fill = NA, label.color = NA,
  #   hjust = 0, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "and Wayne Russell for<br>fear of bears.",
  #   x = 6, y = 1, fill = NA, label.color = NA,
  #   hjust = 0, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 7:</strong><br>Brant McGee retires<br>after drinking salt water.",
  #   x = 8, y = -0.5, fill = NA, label.color = NA,
  #   hjust = 0, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 9:</strong><br>Dustin Feher retires<br>for fear of storm.",
  #   x = 10, y = 0.5, fill = NA, label.color = NA,
  #   hjust = 0, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 40:</strong><br>Lucas Miller retires<br>for feeling happy<br>with his progress.",
  #   x = 37, y = -1.5, fill = NA, label.color = NA,
  #   hjust = 1, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 44:</strong><br>Mitch Mitchell retires<br>to be next to his mother<br>because of her cancer.",
  #   x = 41, y = 0.75, fill = NA, label.color = NA,
  #   hjust = 1, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 56:</strong><br>Sam Larson loses<br>the mind game.",
  #   x = 53, y = 1.5, fill = NA, label.color = NA,
  #   hjust = 1, size = 2.5, family = "Unbounded"
  # ) +
  # geom_richtext(
  #   label = "<strong style='color:#72B01D'>Day 57:</strong><br>Alan Kay is the winner<br>of Alone.",
  #   x = 54, y = -0.5, fill = NA, label.color = NA,
  #   hjust = 1, size = 2.5, family = "Unbounded"
  # ) +
  # labs(
  #   title = "Timeline of the\nfirst season of Alone",
  #   subtitle = "Alone is a TV show by History in which 10 people\ncompete to survive the longest in the wilderness",
  #   caption = "Source: Alone data package | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\nThe images used were downloaded from flaticon.com",
  #   y = NULL,
  #   x = NULL,
  # ) +
  scale_y_continuous(limits = c(-1.75, 2)) +
  theme(
    text = element_text(family = "Unbounded", color = "#141414"),
    plot.title = element_text(family = "Unbounded Black", size = 20,
                              color = "#141414", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    plot.caption = element_text(size = 6.5, hjust = 0.5),
    plot.caption.position = "plot",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"),
    plot.background = element_rect(fill = "#F3EFF5", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
  )

# Export plot

ggsave("2023/2023-week4/plots/plot_w4.png", 
       width = 2000, 
       height = 1800, 
       units = "px")
