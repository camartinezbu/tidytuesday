# TidyTuesday - 2024 Week 6
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggtext)
library(waffle)

# Download and read datasets ----
# Sources: Unesco World Heritage Sites

data <- tidytuesdayR::tt_load(2024, week = 6)$heritage

data_clean <- data |> 
  mutate(country_es = c("Noruega", "Dinamarca", "Suecia")) |> 
  mutate("2022" = `2022` - `2004`) |> 
  pivot_longer(c(`2004`, `2022`), names_to = "year", values_to = "number") |> 
  mutate(label_en = case_when(
    year == 2004 ~ "up to\n2004",
    year == 2022 ~ "added from\n2005 to 2022"
  )) |> 
  mutate(label_en = ordered(label_en, c("up to\n2004", "added from\n2005 to 2022"))) |> 
  mutate(label_es = case_when(
    year == 2004 ~ "hasta\n2004",
    year == 2022 ~ "añadidos de\n2005 a 2022"
  )) |> 
  mutate(label_es = ordered(label_es, c("hasta\n2004", "añadidos de\n2005 a 2022")))

# Plot: ----

source_text <- "UNESCO World Heritage Sites"
highlight_color <- "#0077D4"
text_color <- "#221E1F"
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


ggplot(data = data_clean, aes(label = label_es, values = number)) +
  geom_pictogram(aes(color = label_es), n_rows = 5, size = 6,
                 flip = TRUE, family = "Font Awesome 6 Free") +
  facet_wrap(~country, ncol = 3, strip.position = "bottom") +
  labs(
    title = "Sitios Patrimonio de la Humanidad\nde la UNESCO en los países escandinavos",
    # title = "UNESCO World Heritage sites\nlocated in Scandinavian countries",
    subtitle = "Número de sitios",
    # subtitle = "Number of sites",
    caption = social_caption
  ) + 
  scale_x_discrete(expand = c(0, 1)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_label_pictogram(name = NULL, values = "landmark") +
  scale_color_manual(name = NULL, values = c("#0077D470", "#0077D4")) +
  coord_fixed() +
  theme(
    text = element_text(family = "Work Sans"),
    plot.title = element_text(face = "bold",
                              size = 17, hjust = 0.5,
                              margin = margin(t = 0.5, b = 1.5, unit = "cm")),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, halign = 0.5,
                                    margin = margin(t = 2, unit = "cm")),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_blank(),
    panel.spacing.x = unit(30, "pt"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(),
    legend.position = "top",
    legend.key = element_rect(color = NA, fill = NA)
  )
  
# Export plot

ggsave("2024/2024-week6/plots/plot_2024w6.png", 
       width = 2000, 
       height = 1800, 
       units = "px")