# TidyTuesday - Week 46
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)

# Download and read datasets ----
# Sources
# Webpage Metrics: httparchive.org

bytes_total <- read_csv("2022/2022-week46/data/bytes_total.csv")

speed_index <- read_csv("2022/2022-week46/data/speed_index.csv")

# Plot: Evolution of mobile website metrics. 5 years to today.

# Total Bytes
mobile_bytes <- bytes_total |> 
  filter(date %in% c("2022_10_01", "2018_10_01"), client == "mobile") |>
  select(measure, date, p50, p90) |> 
  pivot_longer(c(p50, p90), names_to = "percentile") |> 
  mutate(year = str_sub(date, 1, 4)) |> 
  mutate(name = paste(percentile, year, sep = "_")) |> 
  mutate(x = c(0.45, 1, -0.57, -1)) |> 
  mutate(label = case_when(
    percentile == "p50" ~ paste0("Peso\nmediano:\n", round(value), " KB"),
    percentile == "p90" ~ paste0("Percentil 90:\n", round(value), " KB")
  )) |> 
  mutate(x_label = c(0.45, 1.4, -0.57, -1.3))

ggplot() +
  geom_point(aes(x = x, size = value, color = year), y = 0, alpha = 0.5,
             data = filter(mobile_bytes, percentile == "p90")) +
  geom_point(aes(x = x, size = value, color = year), y = 0,
             data = filter(mobile_bytes, percentile == "p50")) +
  geom_text(aes(x = x_label, label = label), y = 0.4, size = 2.5,
            data = filter(mobile_bytes, percentile == "p90"),
            family = "Futura Medium") +
  geom_text(aes(x = x_label, label = label), y = 0, size = 2.5,
            data = filter(mobile_bytes, percentile == "p50"),
            family = "Futura Medium") +
  annotate("text", x = -1, y = 1.3, label = "Octubre 2018", 
           family = "Futura Bold", size = 3.5) + 
  annotate("text", x = 1, y = 1.3, label = "Octubre 2022", 
           family = "Futura Bold", size = 3.5) +
  labs(
    title = "El peso de las páginas web móviles\nha incrementado en los últimos 5 años",
    caption = "\nFuente: httparchive.org | Elaborado por Camilo Martínez (@camartinezbu)",
    x = NULL,
    y = NULL
  ) +
  scale_color_manual(
    values = c("#1b998b", "#fb6107")
  ) +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(-1.0, 1.4)) +
  scale_size_area(max_size = 82) +
  coord_equal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Hoefler Text Black", size = 18),
    plot.caption = element_text(hjust = 0.5, family = "Futura Medium", size = 7),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# Export plot

ggsave("2022/2022-week46/plots/plot_w46.png", 
       width =1500, 
       height = 1200, 
       units = "px")
