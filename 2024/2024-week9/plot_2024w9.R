# TidyTuesday - 2024 Week 9
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(stopwords)
library(cmbrand)

# Download and read datatsets ----
# Sources: Wikipedia February 9

data <- tidytuesdayR::tt_load(2024, week = 9)
events <- data$events

events_per_year <- events |> 
  mutate(century = ceiling(year/100)) |> 
  group_by(century) |> 
  count(name = "events_count")

event_words <- events |>
  unnest_tokens(word, event) |> 
  filter(!(word %in% stopwords(source = "snowball"))) |> 
  count(year, word, sort = TRUE)

event_tf_idf <- event_words |> 
  bind_tf_idf(word, year, n)

top_tf_idf_per_century <- event_tf_idf |> 
  mutate(century = ceiling(year/100)) |> 
  group_by(century) |> 
  slice_max(order_by = tf_idf, n = 15, with_ties = FALSE) |> 
  mutate(y = row_number()) |> 
  filter(century > 10) |> 
  left_join(events_per_year, by = "century") |> 
  mutate(century_labels_es = case_when(
    events_count == 1 ~ paste0("Siglo ", as.roman(century), ": ", events_count, " evento"),
    events_count > 1 ~ paste0("Siglo ", as.roman(century), ": ", events_count, " eventos"))
  )

century_labels <- top_tf_idf_per_century |> distinct(century_labels_es) |> pull(century_labels_es)
  
top_tf_idf_per_century_final <- top_tf_idf_per_century |> 
  ungroup() |> 
  mutate(century_es = factor(century, levels = c(16:21), labels = century_labels))


# Create plot
ggplot(top_tf_idf_per_century_final, aes(x = 1, y = y, label = word, color = century_es)) +
  geom_text(fontface = "bold") +
  labs(
    title = "Palabras con mayor puntaje Tf-Idf\npara los eventos ocurridos el\n29 de febrero según Wikipedia",
    caption = social_caption("Wikipedia", text_color = "white", 
                             highlight_color = "grey70")
  ) +
  scale_y_reverse() +
  facet_wrap(~century_es, ncol = 3) +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 20, face = "bold", color = "white",
                              hjust = 0.5, margin = margin(t = 10, b = 15)),
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 10)),
    plot.background = element_rect(color = NA, fill = "grey15"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    strip.text = element_text(face = "bold", color = "white"),
    strip.background = element_rect(color = NA, fill = "grey40")
  )

# Export plot
ggsave("2024/2024-week9/plots/plot_2024w9.png",
    width = 2000,
    height = 1800,
    units = "px")