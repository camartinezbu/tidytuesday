# TidyTuesday - Week 50
# Submission by Camilo Martínez Burgos (@camartinezbu)

# Load packages ----
library(tidyverse)
library(lubridate)
library(ggHoriPlot)

# Download and read datasets ----
# Sources
# Monthly State Retail Sales: United States Census Bureau

tuesdata <- tidytuesdayR::tt_load(2022, week = 50)

state_retail <- tuesdata$state_retail
coverage_codes <- tuesdata$coverage_codes

us_state_retail <- state_retail |> 
  filter(state_abbr == "USA") |> 
  mutate(date = ym(paste(year, month, sep = "-"))) |> 
  mutate(change_yoy = as.numeric(change_yoy)) |> 
  filter(subsector != "total") |> 
  mutate(subsector = ordered(subsector, 
                             levels = c("Clothing and Clothing Accessories",
                                        "Furniture and Home Furnishing",
                                        "Electronics and Appliances",
                                        "Sporting Goods and Hobby",
                                        "Gasoline Stations",
                                        "Motor vehicle and parts dealers",
                                        "Miscellaneous Store Retailers",
                                        "General Merchandise",
                                        "Health and Personal Care",
                                        "Building Materials and Supplies Dealers",
                                        "Food and Beverage"),
                             # labels = c("Clothing and\nClothing Accessories",
                             #            "Furniture and\nHome Furnishing",
                             #            "Electronics and\nAppliances",
                             #            "Sporting Goods\nand Hobby",
                             #            "Gasoline\nStations",
                             #            "Motor vehicle and\nparts dealers",
                             #            "Miscellaneous\nStore Retailers",
                             #            "General\nMerchandise",
                             #            "Health and\nPersonal Care",
                             #            "Building Materials\nand Supplies Dealers",
                             #            "Food and\nBeverage"))
                             labels = c("Pendas de vestir\ny accesorios",
                                        "Muebles\ny hogar",
                                        "Electrónicos y\naparatos",
                                        "Deportes y\npasatiempos",
                                        "Estaciones de\ngasolina",
                                        "Vehículos\ny repuestos",
                                        "Tiendas\nmisceláneas",
                                        "Mercancía\ngeneral",
                                        "Salud y cuidado\npersonal",
                                        "Materiales de\nconstrucción",
                                        "Comida y\nbebidas"))
         )

# Plot: 

ggplot(us_state_retail) +
  geom_horizon(aes(x = date, y = change_yoy, fill = ..Cutpoints..),
               horizonscale = c(150, 100, 50, 25, 10, 0, -10, -25, -50, -100, -150),
               origin = 0) +
  facet_grid(subsector~., switch = "y") +
  labs(
    title = "Variación anual de las ventas al por menor\nen Estados Unidos",
    # title = "Annual growth of retail sales\n in the United States",
    subtitle = "\n\n\n",
    caption = "\nFuente: US Census Bureau | Elaborado por Camilo Martínez (@camartinezbu)",
    # caption = "\nSource: US Census Bureau | Created by Camilo Martínez (@camartinezbu@fosstodon.org)",
    y = NULL,
    x = NULL,
    fill = NULL
  ) +
  scale_fill_manual(
    values = c("#2E5A87FF", "#4D7CA7FF", "#719FC6FF", "#9AC4E1FF", "#DEEBF5FF", "#FFE3DCFF", "#FEAA9AFF", "#F47264FF", "#D53F45FF", "#A90C38FF"),
    labels = c("100%", "50%", "25%", "10%", "0%", "-10%", "-25%", "-50%", "-100%"),
    guide = guide_colorsteps(),
  ) +
  scale_x_date(
    limits = c(ymd("2019-01-01"), ymd("2022-08-01")),
    date_breaks = "2 months", date_labels = "%Y-%m",
    expand = c(0, 0)) +
  theme(
    text = element_text(family = "Bitter"),
    plot.title = element_text(size = 22, family = "Georgia",
                              face = "bold", hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(hjust = 1),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
    axis.ticks.x = element_line(color = "#909090"),
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = c(0.39, 1.1),
    legend.direction = "horizontal",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.text = element_text(size = 7)
  )


# Export plot

ggsave("2022/2022-week50/plots/plot_w50.png", 
       width = 2100, 
       height = 1800, 
       units = "px")
