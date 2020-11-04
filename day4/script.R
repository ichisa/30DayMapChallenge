

library(png)
library(tidyverse)
library(ggplot2)
library(tmap)
library(sf)
library(rnaturalearth)
library(lubridate)


world <- ne_countries(scale = "medium", returnclass = "sf")  #function to get polygons of countries

countries <- c("Chile", "Argentina", "Brazil", "Peru", "Bolivia",
               "Uruguay", "Paraguay","Colombia", "Ecuador","Venezuela",
               "Guyana","Suriname","Guyane", "France", "Falkland Islands")

 
bird <- readPNG("day4/imgs/py_rub.png")
load("day4/data/Pyrocephalus-rubinus.RData")

birds_df <- birds_df %>%
  filter(species_observed & latitude<10) %>%  
  select(latitude, longitude, observation_date) %>% 
  mutate(month=month(observation_date))

birds_df <- sample_n(birds_df, 5000)


theme_xy_blank <-   theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#3b727c",colour = "black"))

map_bird <- world %>% 
  filter(name_long %in% countries) %>% 
  ggplot() +
  # This is the important line where we do not plot normal polygons but sf
  # objects that have a crs associated to it. 
  # Look that we do not have to specify x and y anymore because that info is
  # already in the polygons
  geom_sf(fill = "#838383") + 
  geom_point(data=birds_df, aes(x=longitude, y=latitude, color=month(observation_date))) +
  scale_colour_gradient2( low = "#e6bbad",
                          mid = "#72bcd4",  high = "#e6bbad",  midpoint = 6) +
  theme_xy_blank + labs(col="Month of observation") +
  xlim(-95, -25) + ylim(-55, 20)


map_bird + 
  annotation_raster(bird,-45,-25,-35,-55) +
  # xlim(c(-90, -25)) +
  theme_minimal() +
  theme(panel.background =element_rect(fill = "lightgray", color=NA),
        plot.background = element_rect(fill = "lightgray", color=NA),
        legend.position = c(0.10, 0.33),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title = element_blank()) +
  annotate("text", 
           label="Scarlet flycatcher in South America",
           x= -62, y=20, 
           family="serif", 
           size =12, color = "grey20") +
  annotate("text", label = "Observation Month", x=-85, y=-24, size=8, family="serif") +
  annotate("text", label = "Data Source: e-Bird", x=-45, y=-56, size=4)
