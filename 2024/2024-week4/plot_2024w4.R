# TidyTuesday - 2024 Week 4
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggtext)

# Download and read datasets ----
# Sources: The UK Office for National Statistics

data <- tidytuesdayR::tt_load(2024, week = 4)$english_education

processed_data <- data |>  
  select(size_flag, rgn11nm, income_flag) |> 
  mutate(income_flag = ordered(income_flag, levels = c("Lower deprivation towns",
                                                       "Mid deprivation towns",
                                                       "Higher deprivation towns"))) |> 
  filter(income_flag != "Cities", !is.na(income_flag)) |> 
  group_by(size_flag, rgn11nm) |> 
  count(income_flag) |> 
  mutate(perc = n/sum(n)) |> 
  filter(income_flag == "Higher deprivation towns") |> 
  select(-income_flag) |> 
  mutate(rgn11nm = case_when(
    rgn11nm == "Yorkshire and The Humber" ~ "Yorkshire and\nThe Humber",
    TRUE ~ rgn11nm
  )) |> 
  mutate(size_flag_es = case_when(
    size_flag == "Small Towns" ~ "Pueblos pequeños",
    size_flag == "Medium Towns" ~ "Pueblos medianos",
    size_flag == "Large Towns" ~ "Pueblos grandes",
  ))

# Plot: ----

source_text <- "The UK Office for National Statistics"
highlight_color <- "#F13030"
text_color <- "#22181C"
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
  "<span style='color: {text_color}'>Source: {source_text}</span><br><br>
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

ggplot(data = processed_data, aes(x = rgn11nm, y = size_flag, fill = perc)) +
  geom_tile(color = "#f6e8ea", linewidth = 1.5) + 
  coord_equal() +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#22181C", high = "#F13030",
                      labels = scales::percent) + 
  labs(
    # title = "La región del noreste del Reino Unido\ntiene una mayor proporción de pueblos con\nalta privación de ingresos",
    title = "The North East region of the UK\nhas a greater proportion of towns\nwith high income deprovation",
    caption = social_caption,
    x = NULL,
    y = NULL,
    # fill = "Porcentaje con alta\nprivación de ingresos"
    fill = "Percentage with high\nincome deprivation"
  ) +
  theme(
    text = element_text(family = "Trykker"),
    plot.title = element_text(family = "Martel", face = "bold",
                              size = 17, hjust = 0.5, 
                              margin = margin(b = 60)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, halign = 0.5,
                                    margin = margin(25, 5, 0, 5, unit = "pt")),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#f6e8ea", color = NA),
    plot.margin = margin(0, 60, 10, 60, unit = "pt"),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 0),
    axis.ticks = element_blank(),
    legend.position = c(0.4, 1.775),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.key.height = unit(7, "pt"),
    legend.key.width = unit(30, "pt"),
    legend.title = element_text(size = 10)
  )
  
# Export plot

ggsave("2024/2024-week4/plots/plot_2024w4_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")