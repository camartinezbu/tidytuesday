# TidyTuesday - 2024 Week 1
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(naniar) 

# Download and read datasets ---- 
# Sources: The tracking of my own routines. 

data <- read_csv("./2024/2024-week1/data/data.csv") |> 
  mutate(day_of_week = wday(day, week_start = 1),
         week_of_year = isoweek(day)) |> 
  group_by(year(day)) |> 
  mutate(week_of_year = case_when(
    month(day) == 12 & week_of_year == 1 ~ 53,
    month(day) == 1 & week_of_year >= 52 ~ 0,
    TRUE ~ week_of_year
  )) |> 
  pivot_longer(c(exercise, reading), names_to = "routine") |>
  ungroup() |> 
  mutate(fill = case_when(
    routine == "exercise" & value~ "orange",
    routine == "reading" & value ~ "#00b4d8",
    value == FALSE ~ "grey30"
  )) |> 
  mutate(routine = case_when(
    routine == "exercise" ~ "Ejercicio",
    routine == "reading" ~ "Lectura"
  ))

# Plot: Github-like contribution chart of my own routines.

ggplot(data, aes(x = week_of_year, y = day_of_week, fill = fill)) +
  geom_tile(color = "#0e1117", lwd = 1.25) +
  labs(
    title = "Mis rutinas en 2023",
    y = NULL,
    x = "\nSemana del año",
    caption = "Fuente: Recolección propia | Elaborado por Camilo Martínez (@camartinezbu)") +
  scale_y_reverse(
    breaks = 1:7 - 0.25,
    labels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sab", "Dom")
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(
    values = c("#00b4d8", "grey30", "orange")
  ) +
  coord_equal() +
  facet_grid(rows = vars(routine), switch = "y") +
  theme(
    text = element_text(family = "SF Pro Text", color = "white"),
    plot.background = element_rect(fill = "#0e1117", color = NA),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20, unit = "pt"),
    plot.title = element_text(family = "SF Pro Display", face = "bold", 
                                hjust = 0.5, size = 30, margin = margin(0, 0, 75, 0)),
    plot.title.position = "plot",
    plot.caption = element_text(family = "SF Pro Text", hjust = 0.5,
                                color = "#d0d0d0", size = 8,
                                margin = margin(50, 0, 0, 0)),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "SF Pro Display", face = "bold", size = 8),
    axis.text.y = element_text(family = "SF Pro Display", face = "bold", size = 8),
    axis.title.x = element_text(family = "SF Pro Display", face = "bold",
                                color = "white", size = 8),
    legend.position = "none",
    legend.background = element_rect(fill = "black"),
    strip.background = element_blank(),
    strip.text = element_text(family = "SF Pro Display", face = "bold",
                              color = "white", size = 10),
    strip.placement = "outside"
  ) 

# Export plot
ggsave("2024/2024-week1/plots/plot_2024w1.jpg", height = 2000, width = 2000, units = "px")
