# TidyTuesday - 2024 Week 10
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(tidytuesdayR)
library(cmbrand)
library(ggtext)

# Download and read datatsets ----
# Sources: TrashWheel collection data

data <- tidytuesdayR::tt_load(2024, week = 10)$trashwheel


trash_images <- c(
  "/2024/2024-week10/data/img/captainTW.jpeg",
  "/2024/2024-week10/data/img/HH_GTGWOTW_Illustration-1024x906.png",
  "/2024/2024-week10/data/img/MrTrashWheel-1024x553.png",
  "/2024/2024-week10/data/img/ProfTrashWheel-1024x698.jpeg"
)

trash_weight_summary <- data |> 
  group_by(Name) |> 
  summarise(Total_weight = sum(Weight),
            Min_Year = min(Year)) |> 
  bind_cols(image_url = trash_images) |> 
  mutate(label_es = paste0("<img src='.", image_url, "' width=50 /><br><span style='font-family: Montserrat'><b>", 
                        Name, "</b><br>Desde ", Min_Year, "</span>"
  )) |>  
  mutate(label_en = paste0("<img src='.", image_url, "' width=50 /><br><span style='font-family: Montserrat'><b>", 
                        Name, "</b><br>Since ", Min_Year, "</span>"
  ))

trash_wheel_total <- data |> 
  summarise(sum(Weight)) |> 
  pull()

# Plot
ggplot(data = trash_weight_summary,
       aes(x = Total_weight, y = fct_reorder(label_es, Total_weight))) +
  geom_col(fill = "#7cc04d") +
  geom_text(aes(label = round(Total_weight)), hjust = -0.3,
            family = "Montserrat", fontface = "bold") +
  scale_x_continuous(limits = c(0, 2300), expand = c(0, 0),
                     position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "La iniciativa Mr. Trash Wheel ha recolectado\n2.945 toneladas de basura en Baltimore",
    # title = "Mr. Trash Wheel initiative has collected\n2.945 tons of trash in Baltimore",
    subtitle = "Mr. Trash Wheel es un interceptor de basura semiautónomo que se ubica al final\nde un río o arroyo. Actualmente hay 4 de ellos en funcionamiento.",
    # subtitle = "Mr. Trash Wheel is a semi-autonomous trash interceptor that is placed at the end\nof a river, stream or other outfall. Currently, there are 4 of them functioning.",
    y = NULL,
    x = "Toneladas",
    # x = "Tons",
    caption = cmbrand::social_caption(source = "Mr. Trash Wheel Baltimore Healthy Harbor Initiative",
                                      text_color = "black", highlight_color = "#7cc04d")
  ) +
  theme(
    plot.title = element_text(family = "Montserrat", face = "bold", 
                              size = 18, hjust = 0.5,
                              margin = margin(t = 10, b = 5)),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = "Montserrat", face = "plain",
                                 hjust = 0.5),
    plot.caption = element_markdown(family = "Montserrat", face = "plain",
                                    halign = 0.5, hjust = 0.5,
                                    margin = margin(t = 10, b = 10)),
    plot.caption.position = "plot",
    plot.margin = margin(l = 15, r = 15),
    panel.background = element_blank(),
    axis.title.x = element_text(family = "Montserrat"),
    axis.text.y = element_markdown(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )
  

# Export plot
ggsave("2024/2024-week10/plots/plot_2024w10.png",
       width = 2000,
       height = 1800,
       units = "px")