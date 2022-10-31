# TidyTuesday - Week 43
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(patchwork)

# Download and read datasets ----
# Sources
# Great British Bakeoff: Bakeoff package

bakers <- read_csv("2022/2022-week43/data/bakers.csv") |> 
  select(c(series, baker_first, baker_last,
           total_episodes_appeared, series_winner,
           technical_highest, technical_lowest, technical_median)) |> 
  filter(!is.na(technical_highest),
         !is.na(technical_lowest),
         !is.na(technical_median)) |> 
  group_by(series) |> 
  arrange(series, total_episodes_appeared, series_winner) |> 
  mutate(x = row_number()) |> 
  mutate(technical_lowest_v2 = case_when(
    technical_lowest != technical_median ~ technical_lowest
  )) |> 
  mutate(technical_highest_v2 = case_when(
    technical_highest != technical_median ~ technical_highest
  ))

# challenges <- read_csv("2022/2022-week43/data/challenges.csv")
# episodes <- read_csv("2022/2022-week43/data/episodes.csv")
# ratings <- read_csv("2022/2022-week43/data/ratings.csv")

# Plot: Highest, lowest and median technical results

list_series <- list(
  series_1 = bakers |> filter(series == 1),
  series_2 = bakers |> filter(series == 2),
  series_3 = bakers |> filter(series == 3),
  series_4 = bakers |> filter(series == 4),
  series_5 = bakers |> filter(series == 5),
  series_6 = bakers |> filter(series == 6),
  series_7 = bakers |> filter(series == 7),
  series_8 = bakers |> filter(series == 8),
  series_9 = bakers |> filter(series == 9),
  series_10 = bakers |> filter(series == 10)
)

create_results_chart <- function(dataset) {
  ggplot(dataset) +
    geom_segment(aes(x = x, xend = x,
                     y = technical_lowest,
                     yend = technical_highest)) +
    geom_segment(aes(x = x-0.1, xend = x + 0.1,
                     y = technical_lowest_v2,
                     yend = technical_lowest_v2)) +
    geom_segment(aes(x = x, xend = x,
                     y = technical_highest_v2,
                     yend = technical_highest_v2 - 0.01),
                 arrow = arrow(length = unit(0.125, "cm"),
                               type = "open")) +
    geom_point(aes(x = x, y = technical_median),
               shape = "diamond filled",
               color = "white",
               fill = "#2374D5", size = 1.5) +
    labs(title = paste("Temporada", dataset$series[1]), x = NULL, y = NULL) +
    scale_y_reverse(
      breaks = c(1:13),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_x_continuous(
      breaks = dataset |> pull(x),
      labels = dataset |> pull(baker_first)
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "Kanit"),
      plot.title = element_text(size = 7, face = "bold"),
      axis.text.x = element_text(angle = 90, size = 5, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 4),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#f7f7f7")
    )
}

list_plots <- lapply(list_series,
                     function(dataset) create_results_chart(dataset))

layout <- "
#AAAAAAAAAA#
#AAAAAAAAAA#
BBBBCCCCDDDD
EEEEFFFFGGGG
HHHHIIIIJJJJ
"

list_plots$series_1 + 
  geom_segment(aes(x = 1.3, xend = 1.85, y = 2, yend = 2), color = "black") +
  annotate("label", x = 1.3, y = 2, label = "Mejor", size = 2.5, family = "Kanit") +
  geom_segment(aes(x = 1.3, xend = 1.85, y = 4.5, yend = 4.5), color = "black") +
  annotate("label", x = 1.3, y = 4.5, label = "Mediano", size = 2.5, family = "Kanit") +
  geom_segment(aes(x = 1.3, xend = 1.85, y = 7, yend = 7), color = "black") +
  annotate("label", x = 1.3, y = 7, label = "Peor", size = 2.5, family = "Kanit") +
  labs(y = "Lugar en el desafio técnico",
       x = "Concursantes ordenados según el número de episodios en los que aparecen\n(Más a la izquierda está el primer eliminado y más a la derecha el ganador de la temporada)") +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size =7.5),
    axis.title.y = element_text(size =7.5)) +
  list_plots$series_2 + list_plots$series_3 +
  list_plots$series_4 + list_plots$series_5 + list_plots$series_6 +
  list_plots$series_7 + list_plots$series_8 + list_plots$series_9 +
  list_plots$series_10 + plot_layout(design = layout) +
  plot_annotation(
    title = "Se puede ganar The Great British Bake Off\nsin ser de los mejores al inicio de la temporada",
    subtitle = "En el primer episodio de la sexta temporada, Nadiya Hussain ocupó el último lugar en el\ndesafío técnico. A pesar de eso, se convertiría en la ganadora de la temporada.",
    caption = "Fuente: Paquete {bakeoff} | Elaborado por Camilo Martínez (@camartinezbu)",
    theme = theme(
      text = element_text(family = "Kanit"),
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.margin = margin(t = 10, b = 5, l = 5, r = 5),
      plot.background = element_rect(fill = "#D8EEFD")
    )
  )

# Export plot

ggsave("2022/2022-week43/plots/plot_w43.png", 
       width = 2000, 
       height = 2300, 
       units = "px")
