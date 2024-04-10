# TidyTuesday - 2024 Week 11
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(cmbrand)

# Download and read datatsets ----
# Sources: Fiscal Sponsor Directory

data <- tidytuesdayR::tt_load(2024, week = 11)$fiscal_sponsor_directory

total_sponsors <- nrow(data)
  

perc_small <- data |> 
  mutate(multiple = n_sponsored <= 10) |> 
  count(multiple) |> 
  filter(multiple == TRUE) |> 
  mutate(perc = n / total_sponsors)

# Plot
ggplot(data, aes(n_sponsored)) +
  geom_histogram(fill = "#23c4b6") +
  labs(
    # title = "La mitad de organizaciones registradas\nen el Directorio de Patrocinadores Fiscales\nha patrocinado menos de 10 proyectos",
    # subtitle = "El directorio contiene información de 370 organizaciones que cumplen con este rol,\nincluyendo los criterios de elegibilidad, los tipos de proyecto que patrocinan,\nlos servicios ofrecidos y el modelo con el cual lo hacen.",
    # x = "Número de proyectos patrocinados",
    # y = "Conteo",
    # caption = social_caption("Fiscal Sponsor Directory",
    #                          text_color = "black",
    #                          highlight_color = "#27a599",
    #                          sep_lines = 0)
    title = "Half of the organizations registered in the\nFiscal Sponsor Directory\nhave sponsored less than 10 projects overall",
    subtitle = "The directory contains information about 370 organizations that perform this role,\nincluding the elegibility criteria, the types of projects they sponsor,\nthe services they provide, and the fiscal sponsorship model.",
    x = "No. of sponsored projects",
    y = "Count",
    caption = social_caption("Fiscal Sponsor Directory",
                             text_color = "black",
                             highlight_color = "#27a599",
                             sep_lines = 0, language = "en")
  ) + 
  theme(
    text = element_text(family = "Avenir Next Medium"),
    plot.title = element_text(size = 18, family = "Avenir Next Heavy", 
                              hjust = 0.5, margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.subtitle = element_text(margin = margin(t = 10, b = 20), hjust = 0.5),
    plot.caption = element_markdown(lineheight = 1.2,
                                    margin = margin(t = 20)),
    plot.background = element_rect(fill = "#CBF3F0"),
    panel.grid = element_line(color = "#ffffff80"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  )

# Export plot
ggsave("2024/2024-week11/plots/plot_2024w11_en.png",
    width = 2000,
    height = 1800,
    units = "px")