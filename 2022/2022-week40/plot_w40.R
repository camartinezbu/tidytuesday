# TidyTuesday - Week 40
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(lubridate)
library(ggstream)

# Download and read datasets ----
# Sources
# Tech products: https://components.one/datasets/product-hunt-products

products <- read_csv("2022/2022-week40/data/product_hunt.csv")

open_source <- products |> 
  filter(str_detect(category_tags, "OPEN SOURCE")) |> 
  mutate(category_tags = str_replace_all(category_tags, "\\[", "")) |> 
  mutate(category_tags = str_replace_all(category_tags, "\\]", "")) |> 
  mutate(category_tags = str_replace_all(category_tags, "'", "")) |> 
  mutate(category_tags = as.vector(str_split(category_tags, ",")))

# Plot: Number of most common tags other than open source

category_count_by_year <- open_source |>
  unnest(category_tags) |>
  mutate(category_tags = str_trim(category_tags)) |>
  filter(category_tags != "OPEN SOURCE") |>
  mutate(category_tags = as_factor(category_tags)) |>
  mutate(category_tags = fct_other(category_tags,
                                   keep = c("TECH",
                                            "DEVELOPER TOOLS",
                                            "PRODUCTIVITY",
                                            "GITHUB",
                                            "WEB APP",
                                            "DESIGN TOOLS",
                                            "SOFTWARE ENGINEERING",
                                            "USER EXPERIENCE",
                                            "MAC"),
                                   other_level = "OTHER")) |>
  group_by(year = year(release_date)) |> 
  count(category_tags) |> 
  ungroup() |> 
  mutate(category_tags = fct_relevel(category_tags,
                                     c("DEVELOPER TOOLS",
                                       "TECH",
                                       "PRODUCTIVITY",
                                       "GITHUB",
                                       "WEB APP",
                                       "DESIGN TOOLS",
                                       "SOFTWARE ENGINEERING",
                                       "USER EXPERIENCE",
                                       "MAC", 
                                       "OTHER")))

ggplot(category_count_by_year, aes(x = year,
                                   y = n,
                                   fill = category_tags)) +
  geom_stream(type = "mirror", bw = 1, color = "white", lwd = 0.15) + 
  labs(
    title = "El número de productos Open Source va en aumento",
    subtitle = "Cada vez más productos de tecnología en Product Hunt incluyen la etiqueta 'Open Source'.\nEn la siguiente gráfica se resaltan las etiquetas más comunes asociadas a estos productos.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Fuente: components.one | Elaborado por Camilo Martínez (@camartinezbu)"
  ) +
  scale_x_continuous(breaks = c(2014:2021)) +
  scale_fill_manual(
    labels = c("Herramientas de desarrollo",
               "Tech",
               "Productividad",
               "Github",
               "App Web",
               "Herramientas de diseño",
               "Ingeniería de software",
               "Experiencia de usuario",
               "Mac",
               "Otro"),
    values = c(scales::hue_pal()(9), "#888888")
  ) +
  theme(
    text = element_text(color = "#eeeeee", family = "Cabin"),
    plot.background = element_rect(fill = "#333333", color = NULL),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 9, hjust = 0.5),
    plot.caption = element_text(size = 7),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "#444444"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(color = "#eeeeee", size = 7),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.key.height = unit(4, "mm"),
    legend.key.width = unit(4, "mm"),
    legend.text = element_text(size = 7)
  )

# Export plot

ggsave("2022/2022-week40/plots/plot_w40.png", 
       width = 2000, 
       height = 1600, 
       units = "px")
