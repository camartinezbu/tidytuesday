# TidyTuesday - Week 3
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggwaffle)
library(ggtext)
library(glue)
library(patchwork)

# Download and read datasets ----
# Sources
# Artists: https://saralemus7.github.io/arthistory/

artists <- tidytuesdayR::tt_load(2023, week = 03)$artists

latam_gardner_artists <- artists |> 
  filter(book == "Gardner") |> 
  mutate(from_latam = case_when(
    artist_nationality %in% c("Argentine",
                              "Cuban-American",
                              "Brazilian",
                              "Mexican",
                              "Columbian",
                              "Peruvian",
                              "Cuban",
                              "Uruguayan") ~ TRUE,
    TRUE ~ FALSE
  )) |> 
  distinct(artist_name, artist_nationality, from_latam)

# Plot: Number of latin american artists in the Gardner 

waffle_data <- waffle_iron(latam_gardner_artists, aes_d(group = from_latam), rows = 18)

plot1 <- ggplot(waffle_data, aes(y, x, colour = factor(group))) +
  geom_waffle(tile_shape = "circle", size = 3.5) +
  labs(
    title = glue("<br>De los 334 artistas que han aparecido en<br><em><strong>Art Through the Ages</strong></em> de Helen Gardner<br> entre 1926 y 2020..."),
    x = NULL,
    y = NULL,
  ) +
  # labs(
  #   title = glue("<br>Of the 334 artists who have appeared in<br>Helen Gardner's <em><strong>Art Through the Ages</strong></em><br> between 1926 and 2020..."),
  #   x = NULL,
  #   y = NULL,
  # ) +
  coord_equal(xlim = c(-2, 20), ylim = c(19.5,-2)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(
    values = c("#9197AE", "#F02D3A")
  ) +
  theme(
    plot.title = element_markdown(family = "Avenir Next", hjust = 0.5,
                                  color = "#EFF6EE", size = 19, lineheight = 1.25),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#273043", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

plot2 <- ggplot() +
  labs(
    title = glue("<br>solo 15 nacieron en <br><strong style='color:#F02D3A;'>latinoamérica</strong>."),
    caption = "Fuente: arthistory data package | Elaborado por Camilo Martínez (@camartinezbu)",
  ) +
  # labs(
  #   title = glue("<br>only 15 were born in<br><strong style='color:#F02D3A;'>latin america</strong>."),
  #   caption = "Source: arthistory data package | Created by Camilo Martínez (@camartinezbu@fosstodon.org)",
  # ) +
  theme(
    aspect.ratio = 1/20,
    plot.title = element_markdown(family = "Avenir Next", hjust = 0.5,
                                  color = "#EFF6EE", size = 19, lineheight = 1.25),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Avenir Next", hjust = 0.5,
                                color = "#EFF6EE", size = 8),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#273043", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

plot1/plot2 +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#273043", color = NA)
    )
  )
  

# Export plot

ggsave("2023/2023-week3/plots/plot_w3.png", 
       width = 1800, 
       height = 1800, 
       units = "px")