# TidyTuesday - Week 49
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(sf)
library(rayshader)

# Download and read datasets ----
# Sources
# Elevators: Elevators data package
# NYC BLock map: https://data.cityofnewyork.us/Housing-Development/Department-of-Finance-Digital-Tax-Map/smk3-tmxj

elevators <- tidytuesdayR::tt_load(2022, week = 49)$elevators |> 
  filter(DV_DEVICE_NUMBER != "3P7686")

nyc_map <- sf::st_read("~/Downloads/Digital_Tax_Map_20221225/DTM_Tax_Block_Polygon.shp")

nyc_map_renamed <- nyc_map|> 
  mutate(BORO = case_when(
    BORO == "1" ~ "Manhattan",
    BORO == "2" ~ "Bronx",
    BORO == "3" ~ "Brooklyn",
    BORO == "4" ~ "Queens",
    BORO == "5" ~"Staten Island"
  ))

# ggplot(nyc_map) +
#   geom_sf(aes(fill = BORO), lwd = 0, color = NA)

# Plot: Density of elevators in New York City

elevators_per_block <- elevators |> 
  filter(`Device Status` == "A") |> 
  group_by(Borough, TAX_BLOCK) |> 
  summarise(total_active = n())

elevator_map <- nyc_map_renamed |> 
  left_join(elevators_per_block, by = c("BORO" = "Borough",
                                      "BLOCK" = "TAX_BLOCK")) |> 
  replace_na(list(total_active = 0))

my_plot <- ggplot(elevator_map) +
  geom_sf(aes(fill = total_active), lwd = 0, color = NA) +
  scale_fill_gradientn(colours = c("#F1D0D5",
                                   "#D282A6",
                                   "#D282A6",
                                   "#D282A6",
                                   "#6E4555",
                                   "#6E4555",
                                   "#6E4555",
                                   "#3A3238"), na.value = NA) +
  coord_sf(ylim = c(40.2, 41.15), xlim =c(-74.45, -73.5), crs = "NAD83")  +
  theme(
    plot.background = element_rect(fill = "#FAF1F0"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

plot_gg(my_plot, multicore = TRUE, width = 8, height = 8, 
        solid = FALSE, shadow_darkness = 0.05,
        solidcolor = "#FAF1F0",
        shadow = FALSE, scale = 75,
        windowsize = 1200,
        background = "#ffffff",
        theta = 0, phi = 30, zoom = 0.25)Ele

# Export plot

render_snapshot("2022/2022-week49/plots/plot_w49.png")

