# TidyTuesday - Week 47
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(sf)
library(patchwork)
library(ggfx)

# Download and read datasets ----
# Sources
# Museums in UK: Mapping museums project
# Museums in Bogotá: https://www.ideca.gov.co/recursos/mapas/museos-bogota-dc
# London border: https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
# Bogotá border: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

museums <- tidytuesdayR::tt_load(2022, week = 47)$museums

museums_london <- museums |> 
  filter(`Village,_Town_or_City` == "London")

museums_bog <- read_csv("2022/2022-week47/data/museos_bog.csv",
                        col_types = cols(.default = col_character())) |> 
  mutate(coord_x = str_replace(coord_x, ",", "."),
         coord_y = str_replace(coord_y, ",", ".")) |> 
  mutate(coord_x = as.numeric(coord_x),
         coord_y = as.numeric(coord_y))

london_border <- st_read("2022/2022-week47/data/geometry/london.gpkg", crs = "EPSG:27700") |> 
  st_transform("EPSG:4326")

london_centroid <- st_centroid(london_border) |> 
  st_geometry()

bogota_border <- st_read("2022/2022-week47/data/geometry/Loca.shp", crs = "EPSG:4686") |>
  st_transform("EPSG:4326") |> 
  st_union()

bogota_centroid <- st_centroid(bogota_border) |> 
  st_geometry()

# Plot: Museums in london

my_theme <- theme(
  plot.background = element_rect(fill = "#f7eed2", color = "#f7eed2"),
  plot.title = element_text(hjust = 0.5, family = "Merriweather Black",
                            size = 14),
  plot.subtitle = element_text(hjust = 0.5, family = "Merriweather Black",
                               size = 8),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank()
)

padding_x <- 0.425
padding_y <- 0.275

plot_london <- ggplot(museums_london) +
  with_shadow(geom_sf(data = london_border, fill = "white"), sigma = 8) +
  geom_point(aes(x = Longitude, y = Latitude), 
             size = 0.8, alpha = 0.5, 
             shape = 16, color = "#f71735") +
  labs(
    title = "Londres",
    subtitle = "(Total = 256)"
  ) +
  my_theme +
  coord_sf(xlim = c(london_centroid[[1]][1] - padding_x, london_centroid[[1]][1] + padding_x),
           ylim = c(london_centroid[[1]][2] - padding_y, london_centroid[[1]][2] + padding_y))

plot_bogota <- ggplot(museums_bog) +
  with_shadow(geom_sf(data = bogota_border, fill = "white"), sigma = 8) +
  geom_point(aes(x = coord_x, y = coord_y),
             size = 0.8, alpha = 0.5,
             shape = 16, color = "#38618c") +
  labs(
    title = "Bogotá",
    subtitle = "(Total = 72)"
  ) +
  my_theme +
  coord_sf(xlim = c(bogota_centroid[[1]][1] - padding_x/2, bogota_centroid[[1]][1] + padding_x/2),
           ylim = c(bogota_centroid[[1]][2] - padding_y, bogota_centroid[[1]][2] + padding_y))

plot_london + plot_bogota +
  plot_annotation(
    title = "Hay más de 3 museos en Londres\npor cada museo en Bogotá",
    caption = "Fuente: Mapping Museums Project e IDECA | Elaborado por Camilo Martínez (@camartinezbu)\n\nNota: El mapa de Bogotá incluye solamente las localidades urbanas de la ciudad y\nlos museos registrados en la base de datos del IDECA.",
    theme = theme(
      plot.title = element_text(hjust = 0.5, family = "Merriweather Black", 
                                size = 25, vjust = 0.5),
      plot.caption = element_text(hjust = 0.5, family = "Merriweather",
                                  size = 7, vjust = -0.5),
      plot.background = element_rect(fill = "#f7eed2", color = "#f7eed2"),
    )
  )

# Export plot

ggsave("2022/2022-week47/plots/plot_w47.png", 
       width = 2000, 
       height = 1700, 
       units = "px")
