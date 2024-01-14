# TidyTuesday - 2024 Week 2
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggtext)
library(glue)
library(tidytuesdayR)

# Download and read datasets ----
# Sources: NHL teaml list endpoint & NHL API

data <- tidytuesdayR::tt_load(2024, week = 2)

data$nhl_rosters |> 
  distinct(player_id, position_type, position_code, shoots_catches) |> 
  nrow()

data_ready <- data$nhl_rosters |> 
  distinct(player_id, position_type, position_code, shoots_catches) |> 
  count(position_type, position_code, shoots_catches) |> 
  mutate(x = case_when(
    position_type == "goalies" ~ 0,
    position_type == "defensemen" ~ 0,
    position_type == "forwards" & position_code == "L" ~ -1,
    position_type == "forwards" & position_code == "C" ~ 0,
    position_type == "forwards" & position_code == "R" ~ 1
  )) |> 
  mutate(y = case_when(
    position_type == "goalies" ~ 0.6,
    position_type == "defensemen" ~ 1.25,
    position_type == "forwards" & position_code == "L" ~ 1.75,
    position_type == "forwards" & position_code == "C" ~ 2,
    position_type == "forwards" & position_code == "R" ~ 1.75 
  )) |> 
  group_by(position_type, position_code) |> 
  mutate(percentage = round(n/sum(n) * 100, 1)) |> 
  select(-n) |> 
  pivot_wider(names_from = shoots_catches, values_from = percentage) |> 
  mutate(
    color = case_when(
      L < R ~ "#CC3434",
      TRUE ~ "#4466BB"),
    label_es = case_when(
      L < R ~ paste0("<span>El ", R,
                     "% prefiere<br>jugar con la <b><span style='color:",
                     color, "'>derecha</span></b></span>"),
      TRUE ~ paste0("<span>El ", L,
                    "% prefiere<br>jugar con la <b><span style='color:",
                    color, "'>izquierda</span></b><span>")
    ), 
    label_en = case_when(
      L < R ~ paste0("<span>", R,
                     "% prefers to play<br>with the <b><span style='color:",
                     color, "'>right</span></b> hand</span>"),
      TRUE ~ paste0("<span>", L,
                     "% prefers to play<br>with the <b><span style='color:",
                     color, "'>left</span></b> hand</span>"),
    ),
    position_label_es = case_when(
      position_type == "goalies" ~ "<b>Guardameta</b>",
      position_type == "defensemen" ~ "<b>Defensa</b>",
      position_type == "forwards" & position_code == "L" ~ "<b>Delantero izq.</b>",
      position_type == "forwards" & position_code == "C" ~ "<b>Delantero cent.</b>",
      position_type == "forwards" & position_code == "R" ~ "<b>Delantero der.</b>"
    ),
    position_label_en = case_when(
      position_type == "goalies" ~ "<b>Goalie</b>",
      position_type == "defensemen" ~ "<b>Defensemen</b>",
      position_type == "forwards" & position_code == "L" ~ "<b>Forwards Left</b>",
      position_type == "forwards" & position_code == "C" ~ "<b>Forwards Centre</b>",
      position_type == "forwards" & position_code == "R" ~ "<b>Forwards Right<b>"
    ),
    icon = paste0("<span style='font-family:\"Font Awesome 6 Free\"; color:", color, ";'>&#xf453;</span>"
    )
  )
  
field <- png::readPNG("2024/2024-week2/data/field.png") 

# Plot: ----
source_text <- "NHL team list endpoint, NHL API"
highlight_color <- "#4466BB"
text_color <- "black"
github_icon <- "&#xf09b"
github_username <- "camartinezbu"
twitter_icon <- "&#xe61b"
twitter_username <- "camartinezbu"
linkedin_icon <- "&#xf08c"
linkedin_username <- "camartinezbu"
mastodon_icon <- "&#xf4f6"
mastodon_username <- "camartinezbu"
mastodon_server <- "fosstodon.org"
social_caption <- glue::glue(
  "<span style='color: {text_color}'>Fuente: {source_text}</span><br><br>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{github_icon};</span>
  <span style='color: {text_color}'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{twitter_icon};</span>
  <span style='color: {text_color}'>{twitter_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{linkedin_icon};</span>
  <span style='color: {text_color}'>{linkedin_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{mastodon_icon};</span>
  <span style='color: {text_color}'>{mastodon_username}@<span><span style='color: {text_color}'>{mastodon_server}</span>"
)

ggplot() +
  annotation_custom(rasterGrob(field,
                               width = unit(0.575, "npc"),
                               height = unit(0.9, "npc")),
                               -Inf, Inf, -Inf, Inf) +
  geom_rect(aes(xmin = -2, xmax = 2, ymin = 0, ymax = 2.5),
            color = NA, fill = "white", alpha = 0.9) +
  geom_richtext(data = data_ready, aes(x = x, y = y, label = icon), size = 7,
               fill = NA, label.color = NA) +
  geom_richtext(data = data_ready, aes(x = x, y = y - 0.2, label = label_es),
                fill = NA, label.color = NA) +
  geom_richtext(data = data_ready, aes(x = x, y = y + 0.15, label = position_label_es),
                fill = NA, label.color = NA) +
  scale_x_continuous(limits = c(-2, 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 2.5), expand = c(0,0)) +
  labs(
    title = "La mano que prefieren los jugadores\nde hockey está relacionada\ncon su posición en el campo",
    subtitle = "Datos para 8789 jugadores de la NHL",
    # title = "Hockey player's preferred hand\nvaries according to their\nposition on the field",
    # subtitle = "Data for 8789 players in the NHL",
    caption = social_caption
  ) +
  theme(
    text = element_text(family = "Avenir"),
    plot.title = element_text(family = "Cabin", face = "bold", 
                              size = 25, hjust = 0.5,
                              margin = margin(t = 10, r = 0, b = 5, l = 0, unit = "pt")),
    plot.title.position = "plot", 
    plot.subtitle = element_textbox(size = 12, hjust = 0.5, halign = 0.5),
    plot.caption = element_textbox(size = 10, hjust = 0.5, halign = 0.5),
    plot.caption.position = "plot",
    panel.background = element_blank(), 
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank()
  )
  
# Export plot

ggsave("2024/2024-week2/plots/plot_2024w2_en.png", 
       width = 2000, height = 1800, units = "px")