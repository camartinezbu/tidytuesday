# TidyTuesday - Week 1
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(gganimate)
library(rmapshaper)
library(patchwork)

# Download and read datasets ----
# Sources
# Colombia earthquakes: http://bdrsnc.sgc.gov.co/paginas1/catalogo/index.php
# Colombia Map: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/

colombia <- st_read("~/OneDrive/Mapas/Nacional/ADMINISTRATIVO/MGN_DPTO_POLITICO.shp") |> 
  st_transform(crs = 4326) 

san_andres_y_providencia <- colombia |> 
  filter(DPTO_CCDGO == 88) |> 
  ms_simplify(keep = 0.03)

colombia_simple <- colombia |> 
  ms_simplify(keep_shapes = TRUE, keep = 0.01)

# bbox for continental colombia: -80.586726,-4.695470,-65.107810,13.015185
x_continental <- c(-80.586726, -65.107810)
y_continental <- c(-4.695470, 13.015185)

# bbox for san andres: -81.741015, 12.476746,-81.684211,12.602285
x_san_andres <- c(-81.741015, -81.684211)
y_san_andres <- c(12.476746, 12.602285)

# bbox for providencia: -81.416775,13.313685,-81.335236,13.400033
x_providencia <- c(-81.416775, -81.335236)
y_providencia <- c(13.313685, 13.400033)

earthquakes <- read_excel("2023/2023-week1/data/Catalogo_Sismico.xlsx")

earthquakesGt3 <- earthquakes |> 
  filter(Mag. >= 3) |> 
  filter(!Region %in% c("Océano Pacífico", "Costa Afuera de Centro America",
                        "Sur de Panama", "Mar Caribe", "Cerca de la Costa de Ecuador",
                        "Panama", "Cerca de la Costa de Venezuela", "Lago Maracaibo, Venezuela",
                        "Venezuela", "Norte de Panama", "Cerca de la Costa de Colombia",
                        "Panama-Colombia, Region Fronteriza", "Colombia-Ecuador, Region Fronteriza",
                        "Colombia-Venezuela, Región Fronteriza", "Peru-Ecuador, Region Fronteriza",
                        "Norte Colombia")) |> 
  mutate(fecha_limpia = ymd(str_sub(`Fecha-Hora  (UTC)`, 1, 10)))


# Plot: Animation of earthquakes in Colombia in 2022

# San Andres Map

san_andres_map <- ggplot() +
  geom_sf(data = san_andres_y_providencia) +
  coord_sf(xlim = x_san_andres, ylim = y_san_andres) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Providencia Map

providencia_map <- ggplot() +
  geom_sf(data = san_andres_y_providencia) +
  coord_sf(xlim = x_providencia, ylim = y_providencia) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

anim <- ggplot() +
  geom_sf(data = colombia_simple) +
  geom_point(data = earthquakesGt3, aes(x = `Long(°)`, y = `Lat(°)`, size = Mag.),
             alpha = 0.2, color = "darkred") +
  coord_sf(xlim = x_continental, ylim = y_continental) +
  scale_size(range = c(2, 8)) +
  theme(
    plot.title = element_text(size = 75, hjust = 0.5,
                              family = "Futura Bold", lineheight = 0.85),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 32, hjust = 0.5,
                                 family = "Futura Medium"),
    plot.caption = element_text(size = 22, hjust = 0.5,
                                 family = "Futura Medium"),
    plot.caption.position = "plot",
    plot.margin = margin(t = 30, r = 10, b = 30, l = 10),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.title = element_text(size = 25, family = "Futura Bold", hjust = 1),
    legend.text = element_text(size = 20, family = "Futura Medium"),
    legend.key = element_rect(fill = NA),
    legend.key.height = unit(0.9, "cm")
  ) +
  annotation_custom(
    grob = ggplotGrob(san_andres_map),
    xmin = -76.5,
    xmax = -81,
    ymin = 10,
    ymax = 14
  ) +
  annotation_custom(
    grob = ggplotGrob(providencia_map),
    xmin = -74.5,
    xmax = -80.5,
    ymin = 11,
    ymax = 13
  ) +
  transition_states(fecha_limpia) +
  shadow_mark() +
  labs(
    title = "Sismos en Colombia\ndurante 2022",
    subtitle = "{closest_state}",
    caption = "Fuente: Servicio Geológico Colombiano | Elaborado por Camilo Martínez (@camartinezbu)\nNota: El mapa incluye únicamente los sismos que ocurrieron dentro de las fronteras terrestres del país.",
    size = "Magnitud")
  # labs(
  #   title = "Earthquakes in Colombia\nduring 2022",
  #   subtitle = "{closest_state}",
  #   caption = "Source: Servicio Geológico Colombiano | Created by Camilo Martínez (@camartinezbu@fosstodon)\nNote: The map only includes the earthquakes inside the country's terrestrial border.",
  #   size = "Magnitude")

  

# Export plot

animate(anim, height = 1200, width = 1200, nframes = 100, fps = 20, rewind = FALSE)
anim_save("2023/2023-week1/plots/plot_w1.gif")