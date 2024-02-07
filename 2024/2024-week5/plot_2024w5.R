# TidyTuesday - 2024 Week 5
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggtext)
library(png)
library(grid)

# Download and read datasets ----
# Sources: Groundhog-day.com API

data <- tidytuesdayR::tt_load(2024, week = 5)
predictions <- data$predictions
groundhogs <- data$groundhogs

groundhogs_by_year <- predictions |> 
  group_by(year) |> 
  distinct(id) |> 
  ungroup() |> 
  count(year) |> 
  ungroup()

# Plot: ----

source_text <- "Groundhog-day.com API"
highlight_color <- "#53A548"
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
  "<span style='color: {text_color}'>Source: {source_text}</span><br>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{github_icon};</span>
  <span style='color: {text_color}'>{github_username}</span><br>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{twitter_icon};</span>
  <span style='color: {text_color}'>{twitter_username}</span><br>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{linkedin_icon};</span>
  <span style='color: {text_color}'>{linkedin_username}</span><br>
  <span style='font-family:\"Font Awesome 6 Brands\";
                color: {highlight_color}'>{mastodon_icon};</span>
  <span style='color: {text_color}'>{mastodon_username}@<span><span style='color: {text_color}'>{mastodon_server}</span>"
)

img <- readPNG("2024/2024-week5/data/groundhog.png")
g <- rasterGrob(img, interpolate = TRUE)

ggplot(data = groundhogs_by_year, aes(x = year, y = n)) +
  geom_area(fill = "#4C934C") +
  annotation_custom(g, xmin = 1900, xmax = 1910, ymin = -3, ymax = 11) +
  geom_richtext(x = 1890, y = 65,
                # label = "<span style='font-family:\"Phosphate\"'>Cada vez hay más marmotas<br>que predicen el clima<br> en el Día de la marmota</span>",
                label = "<span style='font-family:\"Phosphate\"'>There's an increasing number of<br>groudhogs that predict the<br>climate on Groundhog day</span>",
                fill = NA, label.color = NA,
                hjust = 0, size = 8) +
  geom_richtext(x = 1890, y = 50,
                # label = "En esta tradición norteamericana se les 'consulta'<br>por la evolución del clima. Si la marmota ve su sombra,<br>significa que habrá 6 semanas más de invierno.",
                label = "In this north american tradition, groundhogs are 'consulted'<br>about the climate. If it ‘sees its shadow’, it means<br>there will be 6 more weeks of winter",
                fill = NA, label.color = NA,
                hjust = 0) +
  geom_richtext(x = 1890, y = 36,
                label = social_caption,
                fill = NA, label.color = NA,
                hjust = 0) +
  scale_y_continuous(position = "right",
                     breaks = seq(10, 70, by = 10),
                     expand = expansion(add = c(0, 3))) +
  scale_x_continuous(expand = expansion(add = c(5, 0)),
                     breaks = seq(1890, 2023, by = 30)) + 
  theme(
    text = element_text(family = "Gill Sans"),
    plot.title = element_text(family = "Martel", face = "bold",
                              size = 17, hjust = 0.5, 
                              margin = margin(b = 60)),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, halign = 0.5),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#D9EDBF", color = NA),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
  
# Export plot

ggsave("2024/2024-week5/plots/plot_2024w5_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")