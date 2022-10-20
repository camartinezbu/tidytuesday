# TidyTuesday - Week 40
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggmosaic)
library(patchwork)

# Download and read datasets ----
# Sources
# Yarn: https://www.ravelry.com/yarns/

yarn <- read_csv("2022/2022-week41/data/yarn.csv")

# Plot: Average rating according to yarn weight
# Source of yarn categories: https://www.craftyarncouncil.com/standards/yarn-weight-system

yarn_stats_by_weight <- yarn |>
  drop_na(grams, rating_average, rating_count, yardage, yarn_weight_name, machine_washable) |> 
  mutate(yarn_weight_name = factor(yarn_weight_name)) |> 
  mutate(yarn_weight_name = fct_collapse(yarn_weight_name,
                                         `Super Fine` = c("Light Fingering", "Fingering"),
                                         Fine = c("Sport"),
                                         Light = c("DK"),
                                         Medium = c("Worsted", "Aran"))) |> 
  filter(!(yarn_weight_name %in% c("Aran / Worsted", "Cobweb", "DK / Sport", "Thread"))) |> 
  mutate(yarn_weight_name = fct_relevel(yarn_weight_name, c("Lace",
                                                            "Super Fine",
                                                            "Fine",
                                                            "Light",
                                                            "Medium",
                                                            "Bulky",
                                                            "Super Bulky",
                                                            "Jumbo"))) |> 
  mutate(yarn_weight_name = fct_drop(yarn_weight_name)) |> 
  mutate(rating_discrete = case_when(rating_average >=4  ~ "Más que 4",
                                     rating_average >= 3 & rating_average < 4 ~ "Entre 3 y 4",
                                     rating_average < 3 ~ "Menos que 3")) |> 
  mutate(rating_discrete = ordered(rating_discrete, 
                                   levels = c("Más que 4",
                                              "Entre 3 y 4",
                                              "Menos que 3")))

plot <- ggplot(yarn_stats_by_weight) +
  geom_mosaic(aes(x = product(yarn_weight_name), fill = rating_discrete), 
              offset = 0.004, alpha = 1) +
  labs(
    title = "Entre más pesados los hilos, menores calificaciones obtienen",
    subtitle = "El eje horizontal describe la distribución según el tipo de hilo y\nel eje vertical la distribución de las calificaciones",
    y = NULL,
    fill = "Calificación",
    caption = "Fuente: ravelry.com | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_x_productlist(expand = c(0.1,0), position = "top",
                      labels = c("Lazo",
                                 "Super Fino",
                                 "Fino",
                                 "Ligero",
                                 "Medio",
                                 "Grueso",
                                 "Super Grueso",
                                 "Jumbo")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(
    values = c("#5F0A87", "#CE77DD", "#EABFCB")
  )+
  theme(
    text = element_text(family = "Indie Flower"),
    plot.title = element_text(size = 18, family = "Pacifico", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    plot.background = element_rect(fill="#FFF4E5", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background = element_blank()
  )

rectangle_data <- data.frame(
  x=c(-1, 1, 1, -1),
  y=c(2, 2, 2.5, 2.5)
)

rectangle <- ggplot(rectangle_data) +
  geom_polygon(aes(x=x, y =y), fill = "#98633b") +
  theme_void()

plot +
  inset_element(rectangle,
                left = -3,
                bottom = 0.788,
                right = 3,
                top = .83,
                on_top = FALSE,
                align_to = "plot") &
  theme(
    plot.background = element_rect(fill = "#FFF4E5", color = NA),
    panel.background = element_blank()
  )

# Export plot

ggsave("2022/2022-week41/plots/plot_w41.png", 
       width = 2000, 
       height = 1600, 
       units = "px")
