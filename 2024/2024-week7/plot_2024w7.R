# TidyTuesday - 2024 Week 7
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(ggtext)
library(ggradar)

# Download and read datasets ----
# Sources: National Retail Federation Valentine's Day Survey Data.

data <- tidytuesdayR::tt_load(2024, week = 7)

gifts_gender <- data$gifts_gender

gifts_gender_clean <- gifts_gender |> 
  select(-SpendingCelebrating)

gifts_gender_clean_es <- gifts_gender_clean |> 
  mutate(Gender = case_when(
    Gender == "Men" ~ "Hombres",
    Gender == "Women" ~ "Mujeres"
  ))

# Plot: ----

source_text <- "National Retail Federation Valentine's Day Survey Data"
highlight_color <- "#a6206a"
text_color <- "black"
tegithub_icon <- "&#xf09b"
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

ggradar(gifts_gender_clean,
        grid.max = 100, grid.min = 0, grid.mid = 50,
        font.radar = "Gill Sans", base.size = 10,
        group.point.size = 3, group.line.width = 0.8,
        # axis.labels = c("Dulces", "Flores", "Joyas",
        #                 "Tarjetas\nde felicitación", "Salidas",
        #                 "Ropa", "Tarjetas\n de regalo"),
        axis.labels = c("Candy", "Flowers", "Jewelry",
                        "Greeting Cards", "Evening Out",
                        "Clothing", "Gift Cards"),
        axis.label.size = 3.5,
        label.gridline.min = FALSE,
        gridline.mid.colour = "grey",
        grid.label.size = 4,
) +
  labs(
    # title = "Los hombres compran flores\ny las mujeres compran dulces\nen San Valentín" ,
    title = "Men buy flowers\nand women buy candy\non Valentine's Day" ,
    caption = social_caption,
    # color = "Porcentaje de gasto en cada regalo"
    color = "Percentage of spending in each gift"
  ) +
  scale_color_manual(
    values = c("#a6206a90", "#2f939590")
  ) + 
  scale_x_continuous(limits = c(-292, 292)) + 
  scale_y_continuous(limits = c(-170, 190)) + 
  theme(
    text = element_text(family = "Gill Sans"),
    plot.title = element_text(family = "Martel", face = "bold",
                              size = 17, hjust = 0.5,
                              margin = margin(t = 43, r = -30, b = 10, l = 30, unit = "pt")),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, halign = 0.5, size = 10),
    plot.caption.position = "plot",
    plot.background = element_rect(color = "#F6B6CE", linewidth = 30),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.background = element_rect(color = NA, fill = NA),
    legend.key = element_rect(color = NA, fill = NA),
    legend.text = element_text(size = 9),
    legend.direction = "horizontal",
    legend.position = c(0.5, 0.925),
    legend.title = element_text(size = 10, color = "black",
                                hjust = 0.5, vjust = -2),
    legend.title.align = 0.5
  ) +
  guides(colour = guide_legend(title.position = "top"))

# Export plot
ggsave("2024/2024-week7/plots/plot_2024w7_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")