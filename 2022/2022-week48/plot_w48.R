# TidyTuesday - Week 48
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(circlize)

# Download and read datasets ----
# Sources
# Matches and goals: FIFA World Cup

wcmatches <- tidytuesdayR::tt_load(2022, week = 48)$wcmatches

create_edges_by_year <- function(cup_year) {
  wcmatches_col <- wcmatches |>
    filter(year == cup_year) |> 
    filter(home_team == "Colombia" | away_team == "Colombia") |>
    select(year, date, country, city, home_team, away_team, home_score, away_score, outcome, win_conditions) |>
    mutate(col_team = case_when(
      home_team == "Colombia" ~ home_team,
      TRUE ~ away_team
    )) |>
    mutate(other_team = case_when(
      home_team != "Colombia" ~ home_team,
      TRUE ~ away_team
    )) |>
    mutate(col_goals = case_when(
      home_team == "Colombia" ~ home_score,
      TRUE ~ away_score
    )) |>
    mutate(other_goals = case_when(
      home_team != "Colombia" ~ home_score,
      TRUE ~ away_score
    ))
  
  edges_goals_by_col <- wcmatches_col |>
    select(from = col_team, to = other_team, value = col_goals)
  
  edges_goals_against_col <- wcmatches_col |>
    select(from = other_team, to = col_team, value = other_goals)
  
  edges <- bind_rows(edges_goals_by_col, edges_goals_against_col)
  
  return(edges)
}

wcmatches_col_1962 <- create_edges_by_year(1962)
wcmatches_col_1990 <- create_edges_by_year(1990) |> 
  mutate(from = case_when(from == "United Arab Emirates" ~ "UAE", 
                          from == "West Germany" ~ "W. Germany",
                          TRUE ~ from)) |> 
  mutate(to = case_when(to == "United Arab Emirates" ~ "UAE",
                        to == "West Germany" ~ "W. Germany",
                        TRUE ~ to))
wcmatches_col_1994 <- create_edges_by_year(1994)
wcmatches_col_1998 <- create_edges_by_year(1998)
wcmatches_col_2014 <- create_edges_by_year(2014)
wcmatches_col_2018 <- create_edges_by_year(2018)

# Plot: Highest, lowest and median technical results

grid_col <- c("Colombia" = "#f7cf46",
              "Soviet Union" = "#bb271a30",
              "Yugoslavia" = "#0c0cf330",
              "Uruguay" = "#648ac130",
              "England" = "#ff9c9c30",
              "Tunisia" = "#d22d2630",
              "Romania" = "#0e2a7a30",
              "Cameroon" = "#34786030",
              "W. Germany" = "#19191930",
              "UAE" = "#99b79830",
              "Switzerland" = "#d92f2130",
              "United States" = "#2933d330",
              "Brazil" = "#387f2230",
              "Greece" = "#7c96ff30",
              "Japan" = "#ac233230",
              "Ivory Coast" = "#e7853130",
              "Senegal" = "#39834730",
              "Poland" = "#c7304130")

create_chord_plot <- function(data, title, order = NULL) {
  
  if (is.null(order)) {
    custom_order <- unique(data$to)
  } else {
    custom_order <- order
  }

  circos.par(gap.after = c(rep(2, nrow(data)/2 -1), rep(10, 2)), start.degree = -3)
  chordDiagram(data, annotationTrack = c("grid"),
               order = custom_order,
               grid.col = grid_col,
               row.col = custom_color)
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1] + 3, CELL_META$sector.index, 
                facing = "bending", 
                niceFacing = TRUE, 
                adj = c(0.5, 0.5),
                cex = 3,
                family = "Kanit")
  }, bg.border = NA)
  circos.clear()
  title(main = title, cex.main = 5, line = 1, 
        family = "Kanit Bold", adj = 0.5)
  
}

png(filename = "2022/2022-week48/plots/plot_w48_en.png",
    width = 1800,
    height = 2000,
    units = "px")

layout(matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8), ncol = 3, byrow = TRUE),
       heights = c(3.5, 5, 5, 1))

plot.new()
text(0.5, 0.75, "Colombia's goals\nin FIFA World Cups", 
     cex = 14, family = "Kanit Black")
text(0.5, 0.25, "Only in 2014 and 2018, Colombia scored more goals than its opponents.",
     cex = 5, family = "Kanit")

create_chord_plot(wcmatches_col_1962, title = "1962")

create_chord_plot(wcmatches_col_1990, title = "1990", order = c("Yugoslavia",
                                                                "UAE",
                                                                "W. Germany",
                                                                "Cameroon",
                                                                "Colombia"))

create_chord_plot(wcmatches_col_1994, title = "1994")
create_chord_plot(wcmatches_col_1998, title = "1998")
create_chord_plot(wcmatches_col_2014, title = "2014")
create_chord_plot(wcmatches_col_2018, title = "2018")

plot.new()
text(0.5, 0.5, "Source: Kaggle | Created by Camilo Martínez (@camartinezbu@fosstodon.org)",
     family = "Kanit Regular", cex = 4)

dev.off()

