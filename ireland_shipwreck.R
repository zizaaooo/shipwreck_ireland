library(tidyverse)
library(tidytuesdayR)
library(sf)
library(rnaturalearth)
library(usethis)
library(showtext)

tuesdata <- tidytuesdayR::tt_load('2026-6-30')
wreck_inventory <- tuesdata$wreck_inventory

font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

#taking the coordinates of Ireland and putting it in the ireland_map sf
ireland_uk_map <- ne_countries(country = c("Ireland", "United Kingdom"), scale = "large", returnclass = "sf")

#setting the colors of the points depending to which ear they belong
era_colors <- c(
  "Before 1600" = "#8B1A1A",
  "1600 - 1799" = "#A0522D",
  "1800 - 1899" = "#2E5D34",
  "1900 - 1918" = "#1B3A6B",
  "1919 - 1946" = "#4A3060"
)


#removing the data that doesn't have the longitude and latitude and don't have a year
wreck_inventory_longlat <- wreck_inventory |> 
  filter(!is.na(longitude) & !is.na(latitude) & !is.na(year)) |> 
  #Now telling R that the longitude and latitude columns should be used as coordinates
  #coords = c("longitude", "latitude"): You are pointing to the two exact numeric columns in your spreadsheet and telling R: "Hey, treat these as X and Y coordinates." (Note: Longitude must always come first because it represents the X-axis).
  #crs = 4326: This tells R that these coordinates are standard GPS degrees (WGS84)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  #creating era variable to know the range of year each shipwreck to which they belong
  mutate(
    era = case_when(
      year < 1600               ~ "Before 1600",
      between(year, 1600, 1799) ~ "1600 - 1799",
      between(year, 1800, 1899) ~ "1800 - 1899",
      between(year, 1900, 1918) ~ "1900 - 1918",
      year > 1918               ~ "1919 - 1946"
    ),
    era = factor(era, levels = names(era_colors)) #here we are telling R to treat the values of the era variable as categories and specifying the order of the categories
  )
  
  
  

my_plot <- ggplot() +
  geom_sf(data = ireland_uk_map, fill = "#f0f0f0", color = "#444444") +
  geom_sf(data = wreck_inventory_longlat, aes(color = era), alpha = 0.7, shape = 16, size = 3) +
  theme_minimal(base_family = "roboto_slab", base_size = 25) +
  labs(
    title = "Shipwreck in Ireland Waters",
    x = "Longitude",
    y = "Latitude",
  ) +
  scale_color_manual(values = era_colors) +
  theme(
    panel.background = element_rect(fill = "#9BBFCC"),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 35),
      face = "bold"
    ),
    axis.title = element_text(
    )
  )

ggsave("my_plot.pdf", plot = my_plot, device = cairo_pdf, width = 25, height = 40, units = "in")

