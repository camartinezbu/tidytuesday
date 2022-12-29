# TidyTuesday - Week 51
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(lubridate)
library(patchwork)

# Download and read datasets ----
# Sources
# Weather Forecast Accuracy: USA National Weather Service

tuesdata <- tidytuesdayR::tt_load(2022, week = 51)

weather_forecasts <- tuesdata$weather_forecasts
cities <- tuesdata$cities
outlook_meanings <- tuesdata$outlook_meanings

dates_vector <- ymd("2021-01-29") + dweeks(0:70)

process_dataset_chicago <- function(dataset, temp = c("high", "low")) {
  weather_forecasts |> 
    filter(city == "CHICAGO", high_or_low == temp) |> 
    mutate(forecast_diff = forecast_temp - observed_temp) |> 
    group_by(year = year(date), week = week(date)) |> 
    summarise(diff_per_week = mean(forecast_diff, na.rm = TRUE)) |> 
    bind_cols(date = dates_vector) |> 
    filter(year == 2022) |> 
    mutate(positive = diff_per_week > 0)
}

weather_forecasts_chicago_high <- weather_forecasts |> 
  process_dataset_chicago("high")

weather_forecasts_chicago_low <- weather_forecasts |> 
  process_dataset_chicago("low")

# Plot: Differences between forecast and observed

plot_high <- ggplot(weather_forecasts_chicago_high) +
  geom_bar(aes(x = date, y = diff_per_week, fill = positive),
           stat = "identity") +
  labs(
    # title = "Temperatura alta",
    # fill = "Dirección",
    # y = "Diferencia",
    # x = "Semana"
    title = "High temperature",
    fill = "Direction",
    y = "Difference",
    x = "Week"
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 weeks", offset = 1),
    date_labels = "%W",
    expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(-5, 5),
    breaks = c(-4, -2, 0, 2, 4),
    labels = scales::label_number(suffix = "ºF")
  ) +
  scale_fill_manual(
    values = c("#E2711D", "#05A3C7"),
    # labels = c("Subestima", "Sobreestima"),
    labels = c("Underestimate", "Overestimate")
  ) +
  theme(
    text = element_text(family = "Publico Text"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.title.position = "plot",
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#dddddd", linetype = 1),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.text.x = element_text(hjust = -0.5, size = 8),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(0.75, 0.95),
    legend.direction = "horizontal",
    legend.background = element_blank()
  )

plot_low <- ggplot(weather_forecasts_chicago_low) +
  geom_bar(aes(x = date, y = diff_per_week, fill = positive),
           stat = "identity") +
  labs(
    # title = "Temperatura baja",
    # y = "Diferencia",
    # x = "Semana"
    title = "Low temperature",
    y = "Difference",
    x = "Week"
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 weeks", offset = 1),
    date_labels = "%W",
    expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(-5, 5),
    breaks = c(-4, -2, 0, 2, 4),
    labels = scales::label_number(suffix = "ºF")
  ) +
  scale_fill_manual(
    values = c("#E2711D", "#05A3C7")
  ) +
  theme(
    text = element_text(family = "Publico Text"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.title.position = "plot",
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#dddddd", linetype = 1),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.text.x = element_text(hjust = -0.5, size = 8),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

plot_high / plot_low +
  plot_annotation(
    # title = "La temperatura observada en Chicago fue mayor\na la pronosticada en el primer semestre de 2022",
    # subtitle = "Diferencia promedio entre temperaturas predichas y observadas por semana.",
    # caption = "Fuente: USA National Weather Service | Elaborado por Camilo Martínez (@camartinezbu)\nNota: Se incluyeron los pronósticos de 12, 24, 36 y 48 horas antes.",
    title = "Observed temperature in Chicago was higher\nthan forecasts in the first semester of 2022",
    subtitle = "Average difference between forecasted and observed temperatures.",
    caption = "Source: USA National Weather Service | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\nNote: The graphs include forecasts done 12, 24, 36 y 48 hours before.",
    theme = theme(
      text = element_text(family = "Publico Text"),
      plot.title = element_text(face = "bold", size = 18),
      plot.background = element_rect(fill = "#f0f0f0")
    )
  )


# Export plot

ggsave("2022/2022-week51/plots/plot_w51_en.png", 
       width = 2000, 
       height = 2300, 
       units = "px")
