# TidyTuesday - Week 44
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(ggfx)
# remotes::install_github("hrbrmstr/ggchicklet")
library(ggchicklet)
library(patchwork)

# Load dataset ----
# Sources
# Horror Movies: https://www.themoviedb.org/

horror <- tidytuesdayR::tt_load(2022, week = 44)$horror_movies

# Plot: Highest, lowest and median technical results ----

view(horror |> 
       group_by(collection_name) |> 
       summarize(total = n()) |> 
       arrange(desc(total)))

paranormal_activity <- horror |> 
  filter(collection_name == "Paranormal Activity Collection") |> 
  mutate(title = ordered(title, 
                         levels = c("Paranormal Activity",
                                    "Paranormal Activity 2",
                                    "Paranormal Activity 3",
                                    "Paranormal Activity 4",
                                    "Paranormal Activity: The Marked Ones",
                                    "Paranormal Activity: The Ghost Dimension",
                                    "Paranormal Activity: Next of Kin"),
                         labels = c("Actividad\nParanormal",
                                    "Actividad\nParanormal 2",
                                    "Actividad\nParanormal 3",
                                    "Actividad\nParanormal 4",
                                    "Actividad\nParanormal:\nLos marcados",
                                    "Arcividad\nParanormal:\nLa dimensión\nfantasma",
                                    "Actividad\nParanormal:\nVínculos\nfamiliares")))

## Screen dimensions ----
screen_xmin = -3
screen_xmax = -screen_xmin
screen_ymin = 0
screen_ymax = (screen_xmax - screen_xmin)*9/21

## Background dimensions ----
background_xmin = screen_xmin * 1.5
background_xmax = screen_xmax * 1.5
background_ymin = screen_ymax * -.8
background_ymax = screen_ymax * 1.45

## Light positions ----
n_lights <- 3

compute_lights_positions <- function(n_lights) {
  left_lights_x <- c()
  left_lights_y <- c()
  
  for (light in c(1:n_lights)) {
    x_coord = background_xmin - light*(background_xmin - screen_xmin)/(n_lights + 1)
    y_coord = background_ymax - light*(background_ymax - screen_ymax)/(n_lights + 1) - 0.45
    left_lights_x[light] <- x_coord
    left_lights_y[light] <- y_coord
  }
  
  left_lights <- tibble(x = left_lights_x, y = left_lights_y)
  
  right_lights_x <- c()
  right_lights_y <- c()
  
  for (light in c(1:n_lights)) {
    x_coord = background_xmax - light*(background_xmax - screen_xmax)/(n_lights + 1)
    y_coord = background_ymax - light*(background_ymax - screen_ymax)/(n_lights + 1) - 0.45
    right_lights_x[light] <- x_coord
    right_lights_y[light] <- y_coord
  }
  
  right_lights <- tibble(x = right_lights_x, y = right_lights_y)
  
  return(list(left = left_lights, right = right_lights))
}

lights <- compute_lights_positions(n_lights)

## Seat positions ----
create_seats_positions <- function(n_seats, row_xmin, row_xmax, row_ymin, row_ymax, offset) {
  seat_xmin <- c()
  seat_xmax <- c()
  seat_ymin <- c()
  seat_ymax <- c()
  
  for (seat in c(1:n_seats)) {
    xmin <-  row_xmin*1.025 - (seat-1)*(row_xmin*1.025-row_xmax*1.025)/n_seats - 2*offset
    xmax <-  row_xmin*1.025 - (seat)*(row_xmin*1.025-row_xmax*1.025)/n_seats + 2*offset
    ymin <-  row_ymin
    ymax <-  row_ymax
    seat_xmin[seat] <- xmin
    seat_xmax[seat] <- xmax
    seat_ymin[seat] <- ymin
    seat_ymax[seat] <- ymax
  }
  
  seats <- tibble(seat_xmin, seat_xmax, seat_ymin, seat_ymax)
  return(seats)
}

first_row <- create_seats_positions(n_seats = 6,
                                    row_xmin = screen_xmin*1.025,
                                    row_xmax = screen_xmax*1.025,
                                    row_ymin = screen_ymin - 0.9,
                                    row_ymax = screen_ymin - 0.2,
                                    offset = -0.01)

second_row <- create_seats_positions(n_seats = 7,
                                     row_xmin = screen_xmin*1.3,
                                     row_xmax = screen_xmax*1.3,
                                     row_ymin = screen_ymin - 1.4,
                                     row_ymax = screen_ymin - 0.5,
                                     offset = -0.015)

third_row <- create_seats_positions(n_seats = 8,
                                    row_xmin = screen_xmin*1.7,
                                    row_xmax = screen_xmax*1.7,
                                    row_ymin = screen_ymin - 2,
                                    row_ymax = screen_ymin - 0.8,
                                    offset = -0.015)

## Speaker positions ----

left_speaker <- tibble(
  x = c(-4.1, -3.95, -3.6, -3.75),
  y = c(1, 1.85, 1.75, 1.125)
)

right_speaker <- tibble(
  x = c(4.1, 3.95, 3.6, 3.75),
  y = c(1, 1.85, 1.75, 1.125)
)

## Plot ----
p <- ggplot() +
  # Background
  geom_rect(mapping = aes(xmin = background_xmin,
                          xmax = background_xmax,
                          ymin = background_ymin, 
                          ymax = background_ymax),
            fill = "black") +
  # Screen
  geom_rect(mapping = aes(xmin = screen_xmin*1.025,
                          xmax = screen_xmax*1.025,
                          ymin = screen_ymin - screen_ymax*0.025, 
                          ymax = screen_ymax*1.025),
            fill = "#222222") +
  geom_rect(mapping = aes(xmin = screen_xmin,
                          xmax = screen_xmax,
                          ymin = screen_ymin, 
                          ymax = screen_ymax),
            fill = "#e0e0e0") +
  # Roof borders
  geom_segment(mapping = aes(x = screen_xmax,
                             xend = background_xmax,
                             y = screen_ymax,
                             yend = background_ymax),
               color = "#222222") +
  geom_segment(mapping = aes(x = screen_xmin,
                             xend = background_xmin,
                             y = screen_ymax,
                             yend = background_ymax),
               color = "#222222") +
  # Left lights
  geom_point(mapping = aes(x = x, y = y), color = "#111111",
             data = lights$left, size = 4) +
  # geom_point(mapping = aes(x = x, y = y), color = "white",
  #            data = lights$left) +
  with_outer_glow(
    geom_point(mapping = aes(x = x, y = y), color = "white",
               data = lights$left, size = 3.5), colour = "#f8f2ca", sigma = 75, expand = 30
  ) +
  # Right lights
  geom_point(mapping = aes(x = x, y = y), color = "#111111",
             data = lights$right, size = 4) +
  # geom_point(mapping = aes(x = x, y = y), color = "white",
  #            data = lights$right) +
  with_outer_glow(
    geom_point(mapping = aes(x = x, y = y), color = "white",
               data = lights$right, size = 3.5), colour = "#f8f2ca", sigma = 75, expand = 30
  ) +
  # Floor
  geom_segment(mapping = aes(x = screen_xmin - 0.125,
                             xend = screen_xmax + 0.125,
                             y = screen_ymin - 0.25,
                             yend = screen_ymin - 0.25),
               color = "#222222") +
  geom_segment(mapping = aes(x = screen_xmin - 0.125,
                             xend = background_xmin,
                             y = screen_ymin - 0.25,
                             yend = background_ymin + 1),
               color = "#222222") +
  geom_segment(mapping = aes(x = screen_xmax + 0.125,
                             xend = background_xmax,
                             y = screen_ymin - 0.25,
                             yend = background_ymin + 1),
               color = "#222222") +
  # Seats
  with_inner_glow(
    geom_rrect(mapping = aes(xmin = seat_xmin, xmax = seat_xmax, ymin = seat_ymin, ymax = seat_ymax),
               fill = "#60100a", data = first_row), colour = "red", sigma = 5
  ) +
  with_inner_glow(
    geom_rrect(mapping = aes(xmin = seat_xmin, xmax = seat_xmax, ymin = seat_ymin, ymax = seat_ymax),
               fill = "#60100a", data = second_row), colour = "red", sigma = 5
  ) +
  with_inner_glow(
    geom_rrect(mapping = aes(xmin = seat_xmin, xmax = seat_xmax, ymin = seat_ymin, ymax = seat_ymax),
               fill = "#60100a", data = third_row), colour = "red", sigma = 5
  ) +
  # Speakers
  geom_polygon(mapping = aes(x = x, y = y), data = left_speaker,
               fill = "#202020") +
  geom_polygon(mapping = aes(x = x, y = y), data = right_speaker,
               fill = "#202020") +
  # Title and subtitle
  annotate("text", label = "Calificaciones de las películas de\nActividad Paranormal\npor los usuarios de TheMovieDB.org", 
           x = 0, y = mean(screen_xmax + 0.25, background_xmax), 
           family = "Storm Gust", color = "white",
           size = 7, lineheight = 0.75) +
  annotate("text", label = "Fuente: themoviedb.org | Elaborado por Camilo Martínez (@camartinezbu)", 
           x = 0, y = background_ymin - 0.15, 
           family = "Storm Gust", color = "white",
           size = 3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(0.1, 0.15))) +
  coord_fixed(xlim = c(-4.5, 4.5)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  ) +
  # Actual plot
  inset_element(
    ggplot(paranormal_activity, aes(x = title, y = vote_average)) +
      geom_col(width = 0.6, fill = "#831d14") +
      geom_text(aes(label = vote_average), vjust = -0.4,
                size = 2, family = "Cabin") +
      scale_y_continuous(limits = c(0, 6.8)) +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        plot.background = element_rect(fill = "#e0e0e0", color = NA),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5.75, family = "Cabin"),
        axis.ticks.x = element_line(linewidth = 0.25),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      ),
    left = 0.17, 
    right = 0.83,
    top = 0.75, 
    bottom = 0.375
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "black"),
      plot.margin = margin(t = 0, b = 0, l = 0, r = 0)
    )
  )

# Export plot

ggsave("2022/2022-week44/plots/plot_w44.png", 
       plot = p,
       width = 1850, 
       height = 1272, 
       units = "px")
