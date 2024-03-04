# TidyTuesday - 2024 Week 8
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(grid)
library(png)
library(cmbrand)

# Download and read datatsets ----
# Sources: R Consortiumm ISC Funded Projects

data <- tidytuesdayR::tt_load(2024, week = 8)$isc_grants

clean_data <- data |> 
  mutate(year_group = paste(year, group, sep = "-"))

# Create plot

logo <- grid::rasterGrob(png::readPNG("2024/2024-week8/data/RConsortium_Vertical_Pantone.png"),
                         interpolate = TRUE)

ggplot(clean_data, aes(x = year_group, y = funded, color = factor(group))) +
  geom_point(alpha = 0.7) +
  annotation_custom(logo, xmin = 12.5, ymin = 115000, xmax = 15.5) +
  labs(
    # title = "R Consortium ha financiado 85 proyectos desde 2016",
    title = "R Consortium has financed 85 projects since 2016",
    # subtitle = "<p>La mayoría de los proyectos fueron financiados en el ciclo de<br><b><span style = 'color: #59abe1;'>primavera</span></b>. Tan solo 34 se seleccionaron en el ciclo de <b><span style = 'color: #ed605d;'>otoño</span></b>.</p>",
    subtitle = "<p>Most grants were awarded on the <b><span style = 'color: #59abe1;'>spring</span></b> cycle. Just 34 projects<br>were financed during the <b><span style = 'color: #ed605d;'>fall</span></b> cycle.</p>",
    caption = social_caption("R Consortium ISC Funded Projects",
                             text_color = "black",
                             highlight_color = "#1351a4",
                             language = "en"),
    # y = "Monto financiado (USD)",
    y = "Grant amount (USD)",
    x = NULL
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(
    values = c("#59abe1", "#ed605d")
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(t = 5, b = 50),
                                     lineheight = 1.1), 
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 40, b = 10)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 20, r = 40, b = 5, l = 40, unit = "pt"),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.5, angle = 90),
    axis.ticks = element_blank(),
    legend.position = "none"
  )


# Export plot
ggsave("2024/2024-week8/plots/plot_2024w8_en.png",
       width = 2000,
       height = 1800,
       units = "px")