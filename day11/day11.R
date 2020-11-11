library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(rmapshaper)



# Load and prepare data ---------------------------------------------------

# Data from https://2mp.conae.gov.ar/index.php/materiales-educativos/material-educativo/coberturas-vectoriales/522-sig-250-del-instituto-geografico-nacional

water_courses <- st_read("day11/data/001_Cursos_De_Agua.shp")

provinces <- st_read("day11/data/provincias.shp")

rivers_arg <- water_courses %>% 
  filter(TIPO == "RIO" & REGIMEN == "PERMANENTE")

rivers_arg <- ms_simplify(rivers_arg)
rivers_arg <- st_transform(rivers_arg, crs="+init=epsg:22185")


# Plot --------------------------------------------------------------------


st_bbox(rivers_arg)
bbox_ar <- st_bbox(c(xmin = 4494595, 
                     xmax = 6136900, 
                     ymax = 7579760, 
                     ymin = 3886050),
                   cst = st_crs(rivers_arg)) %>% st_as_sfc()

tm_shape(rivers_arg, 
         bbox = bbox_ar) +
  tm_lines(col = "#a6cee3",
           lwd = 1, 
           alpha= 0.7) +
tm_layout(bg.color = "black", 
            # inner.margins = c(0,0,.02,0), 
            legend.show = FALSE,
            frame = "#a6cee3",
            frame.lwd = 1,
            frame.double.line = TRUE,
          title = "Ríos permanentes \n de Argentina",
          title.position = c('right', 'bottom'),
          title.size = 1,
          title.color = "#a6cee3") +
tm_format("World_wide") 


