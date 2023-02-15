# TidyTuesday - Week 5
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
# devtools::install_github("teunbrand/elementalist")
library(elementalist)
library(sf)
library(osmdata)
library(patchwork)

# Download and read datasets ----
# Sources
# Pet Cats UK: https://www.datarepository.movebank.org/handle/10255/move.882

tuesdata <- tidytuesdayR::tt_load(2023, week = 5)

cats_uk <- tuesdata$cats_uk
cats_uk_reference <- tuesdata$cats_uk_reference

specific_cat <- cats_uk |> 
  filter(tag_id == "Ares")

specific_cat_locations <- specific_cat |> 
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = 4326)

bbox <- st_bbox(specific_cat_locations)

bbox_streets <- bbox |>  
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk",  "primary", 
                            "secondary", "tertiary", "residential", 
                            "living", "unclassified", "service", "footway")) |> 
  osmdata_sf()

# Plot: Profile for one of the cats as if it was on social media

## Spanish plot ----

panel_theme <- theme(
  plot.background = element_rect(fill = "#f0f0f0", color = NA),
  plot.margin = unit(c(1,1,1,1)*4, "pt"),
  panel.background = element_rect_round(radius = unit(6, "pt"), fill = "white"),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank()
)

name_banner <- ggplot() +
  geom_rect_theme(aes(xmin = 0, xmax = 1, ymin = 0.4, ymax = 1), 
                  fill = "#003459", color = NA) +
  annotate("text", x = 0.5, y = 0.725, label = "Perfil de uno de los gatos del proyecto\nMovebank for Animal\nTracking Data", 
           family = "Bitter SemiBold", size = 8.25, hjust = 0.5, color = "white", lineheight = 0.9) +
  geom_point(aes(x = 0.1, y = 0.325), size = 30, shape = "circle filled", 
             fill = "grey", color = "white", stroke = 2.5) +
  annotate("text", x = 0.19, y = 0.29, label = "Ares", 
           family = "Raleway ExtraBold", size = 5.5, hjust = 0) +
  annotate("text", x = 0.19, y = 0.17, 
           label = "1 de 101 gatos rastreados en el Reino Unido en 2017", 
           family = "Raleway Medium", size = 3, hjust = 0, lineheight = 0.9) +
  geom_rect_theme(aes(xmin = 0.82, xmax = 0.96, ymin = 0.15, ymax = 0.3), 
                  fill = "#003459", color = NA) +
  annotate("text", x = 0.89, y = 0.225, label = "Seguir", 
           family = "Raleway ExtraBold", size = 3.5, hjust = 0.5,
           color = "white") +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme +
  theme(
    elementalist.geom_rect = element_rect_round(radius = unit(6, "pt"))
  )

bio_panel <- ggplot() +
  annotate("text", x = 0.075, y = 0.9, label = "Bio", 
           family = "Raleway ExtraBold", size = 5, hjust = 0) +
  annotate("text", x = 0.125, y = 0.75, label = "Sexo:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.3, y = 0.75, label = "Macho", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.65, label = "Años:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.3, y = 0.65, label = "3", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.55, label = "Horas en casa:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.56, y = 0.55, label = "7.5", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.45, label = "Esterilizado:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.48, y = 0.45, label = "Si", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.35, label = "Comida húmeda:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.625, y = 0.35, label = "Si", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.25, label = "Comida seca:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.52, y = 0.25, label = "Si", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.15, label = "Capturó presas:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.58, y = 0.15, label = "No", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme

locations_map <- ggplot() + 
  geom_sf(data = bbox_streets$osm_lines, alpha = 1, size = 0.05, color = "grey") +
  geom_sf(data = specific_cat_locations, alpha = 0.3, color = "#003459") +
  annotate("text", x = -5.118, y = 50.168, label = "Penryn, UK", size = 3,
           family = "Raleway ExtraBold", hjust = 0) +
  theme(
    panel.background = element_rect(fill = "#f0f0f0"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "#474747"),
    axis.text = element_text(size = rel(0.7)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

post_panel <- ggplot() +
  annotate("text", x = 0.05, y = 0.94, label = "Publicaciones", 
           family = "Raleway ExtraBold", size = 5, hjust = 0) +
  geom_point(aes(x = 0.09, y = 0.83), size = 10, shape = "circle filled", 
             fill = "grey", color = "white", stroke = 2.5) +
  annotate("text", x = 0.16, y = 0.8325, label = "Ares", 
           family = "Raleway ExtraBold", size = 3.5, hjust = 0) +
  annotate("text", x = 0.05, y = 0.745, label = "Ubicaciones registradas entre el 24 y el 30 de junio de 2017.", 
          family = "Raleway Medium", size = 3, hjust = 0) +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme +
  inset_element(locations_map,
                left = 0.05, bottom = 0.03, right = 0.95, top = 0.7)

caption_panel <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Fuente:\nMovebank for Animal tracking Data\n\nElaborado por\nCamilo Martínez (@camartinezbu)", 
           family = "Raleway SemiBold", size = 3, hjust = 0.5) +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme +
  theme(
    panel.background = element_blank()
  )

layout <- c(
  area(t = 1, l = 1, b = 2.8, r = 5),
  area(t = 3, l = 1, b = 4, r = 2.8),
  area(t = 3, l = 3, b = 5, r = 5),
  area(t = 5, l = 1, b = 5, r = 2.8)
)

name_banner + bio_panel + post_panel + caption_panel +
  plot_layout(design = layout) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0")
    )
  )

# Export plot

ggsave("2023/2023-week5/plots/plot_w5.png", 
       width = 2000, 
       height = 1800, 
       units = "px")


## English plot ----

panel_theme <- theme(
  plot.background = element_rect(fill = "#f0f0f0", color = NA),
  plot.margin = unit(c(1,1,1,1)*4, "pt"),
  panel.background = element_rect_round(radius = unit(6, "pt"), fill = "white"),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank()
)

name_banner <- ggplot() +
  geom_rect_theme(aes(xmin = 0, xmax = 1, ymin = 0.4, ymax = 1), 
                  fill = "#003459", color = NA) +
  annotate("text", x = 0.5, y = 0.75, label = "Example cat profile from\nMovebank for Animal Tracking Data", 
           family = "Bitter SemiBold", size = 8.25, hjust = 0.5, color = "white", lineheight = 0.9) +
  geom_point(aes(x = 0.1, y = 0.325), size = 30, shape = "circle filled", 
             fill = "grey", color = "white", stroke = 2.5) +
  annotate("text", x = 0.19, y = 0.29, label = "Ares", 
           family = "Raleway ExtraBold", size = 5.5, hjust = 0) +
  annotate("text", x = 0.19, y = 0.17, 
           label = "1 of 101 tracked cats in the UK during 2017", 
           family = "Raleway Medium", size = 3, hjust = 0, lineheight = 0.9) +
  geom_rect_theme(aes(xmin = 0.82, xmax = 0.96, ymin = 0.15, ymax = 0.3), 
                  fill = "#003459", color = NA) +
  annotate("text", x = 0.89, y = 0.225, label = "Follow", 
           family = "Raleway ExtraBold", size = 3.5, hjust = 0.5,
           color = "white") +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme +
  theme(
    elementalist.geom_rect = element_rect_round(radius = unit(6, "pt"))
  )

bio_panel <- ggplot() +
  annotate("text", x = 0.075, y = 0.9, label = "Bio", 
           family = "Raleway ExtraBold", size = 5, hjust = 0) +
  annotate("text", x = 0.125, y = 0.75, label = "Sex:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.26, y = 0.75, label = "Male", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.65, label = "Years:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.325, y = 0.65, label = "3", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.55, label = "Hours indoors:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.56, y = 0.55, label = "7.5", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.45, label = "Neutered:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.435, y = 0.45, label = "Yes", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.35, label = "Wet food:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.4, y = 0.35, label = "Yes", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.25, label = "Dry food:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.395, y = 0.25, label = "Yes", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  annotate("text", x = 0.125, y = 0.15, label = "Caught prey:", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#003459") +
  annotate("text", x = 0.49, y = 0.15, label = "No", 
           family = "Raleway", fontface = "bold", size = 3.5, hjust = 0, color = "#474747") +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme

locations_map <- ggplot() + 
  geom_sf(data = bbox_streets$osm_lines, alpha = 1, size = 0.05, color = "grey") +
  geom_sf(data = specific_cat_locations, alpha = 0.3, color = "#003459") +
  annotate("text", x = -5.118, y = 50.168, label = "Penryn, UK", size = 3,
           family = "Raleway ExtraBold", hjust = 0) +
  theme(
    panel.background = element_rect(fill = "#f0f0f0"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "#474747"),
    axis.text = element_text(size = rel(0.7)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

post_panel <- ggplot() +
  annotate("text", x = 0.05, y = 0.94, label = "Posts", 
           family = "Raleway ExtraBold", size = 5, hjust = 0) +
  geom_point(aes(x = 0.09, y = 0.83), size = 10, shape = "circle filled", 
             fill = "grey", color = "white", stroke = 2.5) +
  annotate("text", x = 0.16, y = 0.8325, label = "Ares", 
           family = "Raleway ExtraBold", size = 3.5, hjust = 0) +
  annotate("text", x = 0.05, y = 0.745, label = "Recorded locations between June 24 and 30, 2017.", 
           family = "Raleway Medium", size = 3, hjust = 0) +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme +
  inset_element(locations_map,
                left = 0.05, bottom = 0.03, right = 0.95, top = 0.7)

caption_panel <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Source:\nMovebank for Animal tracking Data\n\nCreated by\nCamilo Martínez\n(@camartinezbu@fosstodon.org)", 
           family = "Raleway SemiBold", size = 3, hjust = 0.5) +
  scale_x_continuous(limits = c(0 ,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  panel_theme +
  theme(
    panel.background = element_blank()
  )

layout <- c(
  area(t = 1, l = 1, b = 2.8, r = 5),
  area(t = 3, l = 1, b = 4, r = 2.8),
  area(t = 3, l = 3, b = 5, r = 5),
  area(t = 5, l = 1, b = 5, r = 2.8)
)

name_banner + bio_panel + post_panel + caption_panel +
  plot_layout(design = layout) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0")
    )
  )

# Export plot

ggsave("2023/2023-week5/plots/plot_w5_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")

