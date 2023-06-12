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

politics <- read_excel("politicalparties.xlsx",col_names = F)
colnames(politics) <- c("Gemeinde","party_num", "party", "percentage")
politics <- filter(politics, party_num %in% c(1,2,3,4,31,13))#FDP, CVP, SP, SVP, GLP, GPS

gps_gemeinde <- left_join(filter(politics, party_num == 13),gemeinden_baden, by="Gemeinde")
sp_gemeinde <- left_join(filter(politics, party_num == 3),gemeinden_baden, by="Gemeinde")
glp_gemeinde <- left_join(filter(politics, party_num == 31),gemeinden_baden, by="Gemeinde")
cvp_gemeinde <- left_join(filter(politics, party_num == 2),gemeinden_baden, by="Gemeinde")
fdp_gemeinde <- left_join(filter(politics, party_num == 1),gemeinden_baden, by="Gemeinde")
svp_gemeinde <- left_join(filter(politics, party_num == 4),gemeinden_baden, by="Gemeinde")

municipality_geo <- read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`,gemeinde_pop=`Gesamtbevölkerung`)

gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>%
  filter(!is.na(Gemeinde))
  excess <- 7000
  e_range <- c(min(gemeinden_coords$E_MIN)-excess,max(gemeinden_coords$E_MAX)+excess) 
  n_range <- c(min(gemeinden_coords$N_MIN)-excess,max(gemeinden_coords$N_MAX)+excess)

  map <- raster("Maps/Swiss_500.tif")
  gemeinden_map <- as(extent(e_range[1]-excess,e_range[2]+excess,n_range[1]-excess, n_range[2]+excess),'SpatialPolygons') 
  crs(gemeinden_map) <- crs(map) #Set coordinate system of the new raster

map500 <- raster("Maps/Baden500_excess.tif")%>% #Crop the large relief to just the needed size
  as("SpatialPixelsDataFrame") %>% #Turn into dataframe to plot into ggplot
  as.data.frame() %>%
  rename(relief = `Baden_500`)

baden_map <- function(visual_data, fill_data)
{ 
  ggplot(
  data=visual_data,
  aes(fill=fill_data)
  ) +
  #Map Background
  geom_raster(
    data = map500,
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
  
}
baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung)
