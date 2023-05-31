library(rstudioapi)
library(tidyverse) 
library(dplyr)
library(lintr) # code linting
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(cowplot)
library(rmarkdown)
library(readr)
library(readxl)
library(shiny)
library(magick)
#canton_geo = read_sf("Boundary_Data/g2k23.shp")
#Branch Testing
#country_geo= read_sf("Boundary_Data/g2l23.shp")

municipality_geo <- read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`,gemeinde_pop=`Gesamtbevölkerung`)

gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>%
  filter(!is.na(Gemeinde))
  excess <- 7000
  e_range <- c(min(gemeinden_coords$E_MIN)-excess,max(gemeinden_coords$E_MAX)+excess) 
  n_range <- c(min(gemeinden_coords$N_MIN)-excess,max(gemeinden_coords$N_MAX)+excess)


map <- raster("Maps/Swiss_1000.tif") #Map Background
#Create a raster with the size of the data + excess for better spacial understanding
gemeinden_map <- as(extent(e_range[1],e_range[2],n_range[1], n_range[2]),'SpatialPolygons') 
crs(gemeinden_map) <- crs(map) #Set coordinate system of the new raster

map <- crop(map,gemeinden_map)%>% #Crop the large relief to just the needed size
  as("SpatialPixelsDataFrame") %>% #Turn into dataframe to plot into ggplot
  as.data.frame() %>%
  rename(relief = `Swiss_1000`)


baden_map <- ggplot(
  data=gemeinden_coords,
  aes(fill=Gesamtbevölkerung)
  ) +
  #Map Background
  geom_raster(
    data = map,
    inherit.aes = FALSE,
    aes(x,y,
      alpha=relief 
      #since fill is already used for the data, alpha values are used to paint the map
    ),
  ) +
  scale_alpha(
    name = "",
    range = c(0.9,0),
    guide = F 
  ) +
  geom_sf( #Create the Gemeinden Boundaries
    data = gemeinden_coords,
    color = "transparent",
    size = 0.5) +
  
  scale_fill_viridis(
    option = "magma",
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    direction = -1
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) 
  
ggdraw()+
  draw_image("Kanti_Baden_Logo.png", x=0.4, y=-0.4, scale =0.1)+
  draw_plot(baden_map)



