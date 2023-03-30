# TidyTuesday - Week 8
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)

# Download and read datasets ----
# Sources
# Holywood Age Gap: https://hollywoodagegap.com/

bob_ross <- tidytuesdayR::tt_load(2023, week = 8)$bob_ross

# Plot: Distribution of colors by season ----

load("2023/2023-week8/data/unique_bob_ross_colors.rda")

unique_bob_ross_colors <- unique_bob_ross_colors |> 
  mutate(color_fixed = str_replace(color, "([a-z])([A-Z])", "\\1_\\2")) |>
  mutate(
    color_fixed = case_when(
      color_fixed == "Van_DykeBrown" ~ "Van_Dyke_Brown",
      TRUE ~ color_fixed
    )
  ) |> 
  select(color_fixed, color_hex)

bob_ross_clean <- bob_ross |> 
  group_by(season) |> 
  summarise(across(.cols = c(Black_Gesso:Alizarin_Crimson), sum)) |> 
  pivot_longer(cols = c(Black_Gesso:Alizarin_Crimson),
             names_to = "Color", values_to = "Color_used") |> 
  left_join(unique_bob_ross_colors, by = c("Color" = "color_fixed")) |> 
  filter(Color != "Liquid_Clear")


ggplot(bob_ross_clean, aes(x = season, y = Color_used, fill = color_hex)) + 
  geom_bar(position = "fill", stat = "identity", width = 1.01) +
  # labs(
  #   title = "\"Es dificil ver las cosas cuando estás\nmuy cerca. Da un paso atrás y mira.\"",
  #   subtitle = "Distribución de colores en las pinturas de Bob Ross por temporada",
  #   y = NULL,
  #   x = "Temporada",
  #   caption = "Fuente: Bob Ross Colors data package | Elaborado por Camilo Martínez (@camartinezbu)\nNota: Se excluye el pigmento Liquid Clear"
  # ) + 
  labs(
    title = "\"It’s hard to see things when you are\ntoo close. Take a step back and look.\"",
    subtitle = "Distribution of colors used in Bob Ross paintings by season",
    y = NULL,
    x = "Season",
    caption = "Source: Bob Ross Colors data package | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\nNote: Does not include Liquid Clear"
  ) + 
  scale_fill_identity() +
  scale_y_continuous(expand = expansion(add = c(0.5,0.4)), trans = "reverse") +
  scale_x_continuous(expand = expansion(add = c(3,3)), breaks = 1:31) +
  theme(
    text = element_text(family = "Yanone Kaffeesatz Medium", color = "white"),
    plot.background = element_rect(fill = "grey7", color = NA),
    plot.margin = margin(t = 30, r = 20, b = 20, l = 20, unit = "pt"),
    plot.title = element_text(family = "Yanone Kaffeesatz SemiBold", size = 30, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 10, "pt")),
    plot.caption = element_text(hjust = 0.5),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Yanone Kaffeesatz SemiBold", color = "grey70", vjust = 36),
    axis.text.y = element_blank(),
    axis.title.x = element_text(family = "Yanone Kaffeesatz SemiBold", vjust = 30),
    panel.grid = element_blank()
  )


# Export plot

ggsave("2023/2023-week8/plots/plot_8_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")
    
    
    