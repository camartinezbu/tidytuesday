# TidyTuesday - 2024 Week 12
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
# devtools::install_github("teunbrand/elementalist")
library(elementalist)
library(ggtext)
library(cmbrand)
library(patchwork)

# Download and read datatsets ----
# Sources: Mutant Moneyball: a data driven ultimate X-men

data <- tidytuesdayR::tt_load(2024, week = 12)$mutant_moneyball

PPI_ebay_clean <- data |> 
  select(Member, matches("^PPI.*_ebay$")) |> 
  pivot_longer(cols = -c("Member"), values_to = "PPI",
               names_to = "Decade") |> 
  mutate(Decade = str_extract(Decade, "\\d+s")) |> 
  mutate(PPI = as.double(str_replace(PPI, "\\$", "")))

# Plot
main_card <- ggplot() +
  # Card polygon
  elementalist::geom_rect_theme(aes(xmin = 0, xmax = 1, ymin = 0.35, ymax = 1),
                                linejoin = "round", fill = "grey90") +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0.85, ymax = 0.95),
                          color = NA, fill = "#052542") +
  annotate("text", x = 0.5, y = 0.9, hjust = 0.42, vjust = 0.5,
           label = "Xavier's School for Gifted Youngsters",
           color = "white", family = "Copperplate", size = 6) +
  # Xmen icon
  elementalist::geom_rect_theme(aes(xmin = 0.0225, xmax = 0.1325, ymin = 0.845, ymax = 0.955),
                                element = element_rect_round(radius = unit(22, "pt")),
                                linejoin = "round", fill = "#f1c232", color = NA) +
  elementalist::geom_rect_theme(aes(xmin = 0.0375, xmax = 0.1175, ymin = 0.86, ymax = 0.94),
                                element = element_rect_round(radius = unit(17, "pt")),
                                linejoin = "round", fill = "#052542", color = NA) +
  geom_polygon(data = data.frame(
    x = c(0, 0.119, 0.5, 0.881, 1, 0.619, 1, 0.881, 0.5, 0.119, 0, 0.381)/15 + 0.045,
    y = c(0.119, 0, 0.381, 0, 0.119, 0.5, 0.881, 1, 0.619, 1, 0.881, 0.5)/15 + 0.865),
    aes(x = x, y = y), fill = "#f1c232"
  ) + 
  # Person photo 
  elementalist::geom_rect_theme(aes(xmin = 0.075, xmax = 0.325, ymin = 0.55, ymax = 0.8),
                                fill = "grey50", linejoin = "round",
                                element = element_rect_round()) +
  # annotate("text", x = 0.2, y = 0.49, label = "Nombre del",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  # annotate("text", x = 0.2, y = 0.47, label = "miembro",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  annotate("text", x = 0.2, y = 0.49, label = "Name of",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.2, y = 0.47, label = "member",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.2, y = 0.43, label = data$Member[26],
           family = "Copperplate", color = "black",
           size = 5, hjust = 0.5,  vjust = 0.5) +
  # Stats
  # annotate("text", x = 0.525, y = 0.49, label = "Número de",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  # annotate("text", x = 0.525, y = 0.47, label = "historietas",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  annotate("text", x = 0.525, y = 0.49, label = "Number of",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.525, y = 0.47, label = "issues",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.525, y = 0.43, label = data$TotalIssues[26],
           family = "Copperplate", color = "black",
           size = 8) +
  # annotate("text", x = 0.525, y = 0.4, label = "apariciones",
  #          family = "Copperplate", color = "black",
  #          size = 3) +
  annotate("text", x = 0.525, y = 0.4, label = "appearances",
           family = "Copperplate", color = "black",
           size = 3) +
  # annotate("text", x = 0.8, y = 0.49, label = "Valor total",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  # annotate("text", x = 0.8, y = 0.47, label = "de ventas",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  annotate("text", x = 0.8, y = 0.49, label = "Total sales",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.8, y = 0.47, label = "value",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.8, y = 0.43, label = paste0("$", round(data$TotalValue_heritage[26]/1000000, 2), "MM"),
           family = "Copperplate", color = "black",
           size = 6) +
  # annotate("text", x = 0.8, y = 0.4, label = "en Heritage",
  #          family = "Copperplate", color = "black",
  #          size = 3) +
  annotate("text", x = 0.8, y = 0.4, label = "in Heritage",
           family = "Copperplate", color = "black",
           size = 3) +
  annotate("text", x = 0.8, y = 0.38, label = "Auctions",
           family = "Copperplate", color = "black",
           size = 3) +
  # Graph title
  # annotate("text", x = 0.65, y = 0.8, label = "Valores promedio de comics en Ebay",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  # annotate("text", x = 0.65, y = 0.78, label = "por década",
  #          family = "Copperplate", fontface = "bold", color = "#052542",
  #          size = 4) +
  annotate("text", x = 0.65, y = 0.8, label = "Average price per issue in Ebay",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  annotate("text", x = 0.65, y = 0.78, label = "per decade",
           family = "Copperplate", fontface = "bold", color = "#052542",
           size = 4) +
  labs(
    x = NULL, y = NULL,
    caption = social_caption(source = "Mutant moneyball: a data driven ultimate X-men by Anderson Evans",
                             text_color = "black", highlight_color = "#052542",
                             sep_lines = 0, language = "en")
  ) +
  coord_fixed() +
  theme(
    elementalist.geom_rect = element_rect_round(radius = unit(6, "pt")),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 30),
                                    lineheight = 1.25, family = "Montserrat"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

# Bar plot PPI
ebay_ppi_plot <- ggplot(PPI_ebay_clean |> filter(Member == "charlesXavier"),
       aes(x = Decade, y = PPI)) +
  geom_col(width = 0.7, fill = "#052542") +
  geom_text(aes(label = paste0("$", PPI), y = PPI + 50), size = 3,
            family = "Montserrat", fontface = "bold") +
  labs(
    x = NULL, y = "USD" 
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_text(family = "Montserrat", size = 9),
  )

## Full plot

layout <- c(
  area(1, 1, 10, 10),
  area(5, 5, 6.5, 9)
)

# Export plot
ggsave("2024/2024-week12/plots/plot_2024w12_en.png",
       plot =  main_card + ebay_ppi_plot + plot_layout(design = layout),
       width = 2000,
       height = 1800,
       units = "px")