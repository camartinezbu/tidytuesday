# TidyTuesday - 2024 Week 3
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(lubridate)
library(readxl)
library(mapdata)

# Download and read datasets ----
# Sources: Center for Public Integrity & US Census Bureau

data <- tidytuesdayR::tt_load(2024, week = 3)$polling_places

view(data |> 
  distinct(state))

polling_places_ca_per_county <- data |> 
  filter(state == "CA") |> 
  mutate(county_name = case_when(
    county_name == "Contra Coasta" ~ "Contra Costa",
    TRUE ~ county_name
  )) |> 
  count(county_name, name = "num_poll_places")

pop_by_county <- read_xlsx("2024/2024-week3/data/co-est2022-pop-06.xlsx") |> 
  select(1, 3) |> 
  slice(5:(n() - 5)) |> 
  rename("county_name" = "table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)", 
         "pop_2020" = "...3") |> 
  mutate(county_name = str_replace(county_name, "County, California", "")) |> 
  mutate(county_name = str_replace(county_name, "\\.", "")) |> 
  mutate(county_name = str_trim(county_name)) |> 
  mutate(pop_2020 = as.numeric(pop_2020))

final_data <- polling_places_ca_per_county |> 
  left_join(pop_by_county, by = "county_name") |> 
  mutate(poll_places_per_thousand = (num_poll_places / pop_2020) * 100000) |> 
  mutate(county_name = str_to_lower(county_name))

ca_map <- map_data("state") |> 
  filter(region == "california")

ca_counties_map <- map_data("county") |> 
  filter(region == "california") |> 
  left_join(final_data, by = c("subregion" = "county_name"))

total_ca_pop <- sum(pop_by_county$pop_2020)
total_ca_poll <- sum(polling_places_ca_per_county$num_poll_places)
total_ca_poll_per_pop <- round((total_ca_poll/total_ca_pop) * 100000, 2)
# Plot: ----

source_text <- "Center for Public Integrity<br>& US Census Bureau"
highlight_color <- "#17BEBB"
text_color <- "#E1EFE6"
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
  "<span style='color: {text_color}'>Source:<br>{source_text}</span><br><br>
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

ggplot() +
  geom_polygon(data = ca_counties_map, aes(x = long, y = lat, group = group,
                                           fill = poll_places_per_thousand),
               color = "white", linewidth = 0.05) +
  geom_polygon(data = ca_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") +
  geom_richtext(aes(x = -112.5,
                    y = 41,
                    # label = "En las elecciones de 2020,<br>California tuvo 9.7 puestos<br>de votación por cada<br>100.000 habitantes"),
                    label = "In the 2020<br>elections California<br>had 9.7 voting locations<br>per 100.000 inhabitants"),
                fill = NA, label.color = NA, text.color = "#E1EFE6",
                family = "Raleway Black", size = 5, hjust = 1) +
  coord_quickmap(xlim = c(-125, -112.5)) +
  geom_richtext(aes(x = -125,
                    y = 33.75,
                    label = social_caption),
                fill = NA, label.color = NA, text.color = "#E1EFE6",
                family = "Raleway", size = 3, hjust = 0) +
  geom_richtext(aes(x = -112.5,
                    y = 37.5,
                    # label = "El condado de Alpine lidera<br>con 83 puestos por cada<br>100.000 hab."),
                    label = "Alpine county leads in this<br>metric with 83 locations<br>per 100.000 inhabitants"),
                fill = NA, label.color = NA, text.color = "#E1EFE6",
                family = "Raleway", size = 3, hjust = 1) +
  geom_segment(aes(x = -119.8, xend = -116.25, y = 38.6, yend = 37.8), color = "#E1EFE6") +
  labs(
    # fill = "Lugares de\nvotación por\n100.000 hab.",
    fill = "Voting\nplaces per\n100.000 inhabitants"
  ) + 
  scale_fill_gradient(high = "#17BEBB", low = "#042525") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey10"),
    plot.margin = margin(t = 10, b = 10, l = 20, r = 20),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.775, 0.7),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.text = element_text(color = "#E1EFE6", size = 8),
    legend.title = element_text(color = "#E1EFE6", size = 9,
                                family = "Raleway", vjust = 3.5),
    legend.key.height = unit(6, "pt"),
  )

# Export plot
ggsave("2024/2024-week3/plots/plot_2024w3_en.png", 
       width = 2000, 
       height = 1800, 
       units = "px")  