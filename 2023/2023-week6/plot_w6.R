# TidyTuesday - Week 5
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(glue)

# Download and read datasets ----
# Sources
# Big Tech Stock prices: https://www.kaggle.com/datasets/evangower/big-tech-stock-prices

stocks <- read_csv("2023/2023-week6/data/big_tech_stock_prices.csv")

# Plot: Apple stock performance as if it was on Github ----

all_days <- tibble(
  date = seq(as_date("2018-01-01"), as_date("2022_12_31"), by = "day")
  ) |> 
  mutate(day_of_week = wday(date, week_start = 1),
         week_of_year = isoweek(date)) |> 
  group_by(year(date)) |> 
  mutate(week_of_year = case_when(
    month(date) == 12 & week_of_year == 1 ~ 53,
    month(date) == 1 & week_of_year == 53 ~ 0,
    TRUE ~ week_of_year
  ))
  

apple_stock <- stocks |> 
  filter(stock_symbol == "AAPL") |> 
  select(date, volume)

final_data <- all_days |> 
  left_join(apple_stock, by = "date") |> 
  filter(!day_of_week %in% c(6, 7))

# my_palette <- colorRampPalette(c("#161b22", "#39d353"))
my_palette <- colorRampPalette(c("#161b22", "#306fDB"))

ggplot(final_data, aes(x = week_of_year, y = day_of_week, fill = volume)) +
  geom_tile(color = "#0e1117", lwd = 1) +
  labs(
    title = glue::glue("Si el volumen de acciones de <b><span style='color:#306fDB'>Apple</span></b><br>transadas en bolsa se viera como un<br>gráfico de contribuciones de <b><span style='color:#9370db'>Github</span></b>"),
    y = NULL,
    x = "\nSemana del año",
    fill = "Millones de Acciones transadas",
    caption = "\n\nFuente: Kaggle | Elaborado por Camilo Martínez (@camartinbezbu)\nLos cuadros grises representan los días en los que no se transó la acción"
  ) +
  # labs(
  #   title = glue::glue("If the volume of <b><span style='color:#306fDB'>Apple</span></b> stocks<br>traded in the market looked like<br>a <b><span style='color:#9370db'>Github</span></b> contribution plot"),
  #   y = NULL,
  #   x = "\nWeek of the year",
  #   fill = "Millions of stocks traded",
  #   caption = "\n\nSource: Kaggle | Created by Camilo Martínez (@camartinbezbu@fosstodon.org)\nThe gray squares represent days in which the stock was not traded"
  # ) +
  scale_y_reverse(
    breaks = 1:5 - 0.25,
    labels = c("Lun", "Mar", "Mié", "Jue", "Vie")
  ) +
  # scale_y_reverse(
  #   breaks = 1:5 - 0.25,
  #   labels = c("Mon", "Tue", "Wed", "Thu", "Fri")
  # ) +
  scale_x_continuous(expand = c(0,0)) +
  coord_equal() +
  facet_grid(rows = vars(year(date)), switch = "y") +
  scale_fill_gradient2(low = my_palette(50)[1],
                       mid = "#161b22",
                       high = my_palette(50)[length(my_palette(50))],
                       midpoint = 0,
                       labels = scales::label_number(scale = 1/1000000),
                       guide = guide_colorbar(barwidth = unit(275, "pt"),
                                              barheight = unit(5, "pt"),
                                              title.position = "top")) +
  theme(
    text = element_text(family = "SF Pro Text", color = "white"),
    plot.background = element_rect(fill = "#0e1117", color = NA),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20, unit = "pt"),
    plot.title = element_markdown(family = "SF Pro Display", face = "bold", hjust = 0.5, size = 20),
    plot.title.position = "plot",
    plot.caption = element_text(family = "SF Pro Text", hjust = 0.5,
                                color = "#d0d0d0", size = 8),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "SF Pro Text", size = 6),
    axis.text.y = element_text(family = "SF Pro Text", size = 7),
    axis.title.x = element_text(family = "SF Pro Display", face = "bold",
                                color = "white", size = 7),
    legend.background = element_blank(),
    legend.title = element_text(size = 9, hjust = 0),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(t = 15,b = 15, unit = "pt"),
    legend.justification = "center",
    strip.background = element_blank(),
    strip.text = element_text(family = "SF Pro Display", face = "bold",
                              color = "white", size = 10),
    strip.placement = "outside"
  )


# Export plot

ggsave("2023/2023-week6/plots/plot_w6.png", 
       width = 2000, 
       height = 1800, 
       units = "px")


