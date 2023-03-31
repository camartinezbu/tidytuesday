# TidyTuesday - Week 9
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(wordcloud2)
library(cowplot)

# Download and read datasets ----
# Sources
# African Languages Sentiment: https://github.com/afrisenti-semeval/afrisent-semeval-2023
# Common Swahili stopwords: https://data.mendeley.com/datasets/mmf4hnsm2n/1 

tuesdata <- tidytuesdayR::tt_load(2023, week = 9)

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

# Plot: Wordcloud in the shape of africa ----

stopwords_swa <- read_csv("2023/2023-week9/data/Common Swahili Stop-words.csv")

swahili_bow <- afrisenti |> 
  filter(language_iso_code == "swa") |> 
  unnest_tokens(output = word, input = tweet) |> 
  anti_join(stopwords_swa, by = c("word" = "StopWords")) |> 
  count(label, word, sort = TRUE)

swahili_positive <- swahili_bow |> filter(label == "positive") |> 
  select(-label) |> 
  rename(freq = n)

wordcloud2(swahili_positive, 
           figPath = "2023/2023-week9/data/KEN_MOZ_TAN.jpg", 
           ellipticity = .9, gridSize = 1)

## I opened it a browser and then took a screeenshot

img <- "2023/2023-week9/data/Screenshot.jpg"

qplot(0:10, 0:10, geom="blank") +
  draw_image(img, x=0, y = 0, width = 10, height = 10) +
  # labs(
  #   title = "Palabras más comunes en una muestra\nde tweets clasificados como positivos\nen swahili",
  #   caption = "Fuente: Afrisenti | Elaborado por Camilo Martínez (@camartinezbu)\nNota: La silueta representa Kenia, Tanzania y Mozambique, países en los que se habla swahili."
  # ) +
  labs(
    title = "Most common words in a sample\nof tweets classified as positive\nin swahili",
    caption = "Source: Afrisenti | Created by Camilo Martínez (@camartinezbu@fosstodon.org)\nNote: La silhouette corresponds to Kenya, Tanzania y Mozambique, countries that speak swahili."
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    text = element_text(family = "DIN Alternate"),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(family = "Futura", face = "bold", size = 20,
                              hjust = 0.5, margin = margin(t = 10, "pt")),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

# Export plot

ggsave("2023/2023-week9/plots/plot_w9_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")
    
    
    