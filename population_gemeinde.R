library(rstudioapi)
library(tidyverse) 
library(dplyr)
library(lintr) # code linting
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(cowplot)
library(readr)
library(readxl)
library(shiny)
library(magick)
#canton_geo = read_sf("Boundary_Data/g2k23.shp")
#country_geo= read_sf("Boundary_Data/g2l23.shp")

politics <- read_excel("politicalparties.xlsx",col_names = F)
colnames(politics) <- c("Gemeinde","party_num", "party", "percentage") #name the columns
politics <- filter(politics, party_num %in% c(1,2,3,4,31,13))# Filter for only these parties: FDP, CVP, SP, SVP, GLP, GPS

  politics_improved <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(politics_improved) <- c("Gemeinde", "FDP", "CVP", "SP", "SVP", "GPS", "GLP")
  for (i in 1:nrow(politics)){
    if (i%%6==0) {
      row <- c(politics[i,1])
      for (party in 1:6) {
        row <- append(row, politics[i+party-6,4])
      }
      politics_improved[nrow(politics_improved)+1,] <- row
    }
  }
  
#make a database consisting only of 1 parties
#gps_gemeinde <- filter(politics, party_num == 13)
#sp_gemeinde <- filter(politics, party_num == 3)
#glp_gemeinde <- filter(politics, party_num == 31)
#cvp_gemeinde <- filter(politics, party_num == 2)
#fdp_gemeinde <- filter(politics, party_num == 1)
#svp_gemeinde <- filter(politics, party_num == 4)

municipality_geo <- read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`,gemeinde_pop=`Gesamtbevölkerung`)

gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>%
  filter(!is.na(Gemeinde))
  excess <- 2000
  e_range <- c(min(gemeinden_coords$E_MIN)-excess,max(gemeinden_coords$E_MAX)+excess) 
  n_range <- c(min(gemeinden_coords$N_MIN)-excess,max(gemeinden_coords$N_MAX)+excess)

  #map <- raster("Maps/Swiss_500.tif")
  #gemeinden_map <- as(extent(e_range[1]-excess,e_range[2]+excess,n_range[1]-excess, n_range[2]+excess),'SpatialPolygons') 
  #crs(gemeinden_map) <- crs(map) #Set coordinate system of the new raster
  #map500_excess <- crop(map, gemeinden_map)
  #writeRaster(map500_excess, "Baden500_excess.tif", format="GTIFF")
  #Programme to write tiff file as the swiss wide file is too large for github
  
map500 <- raster("Baden500_excess.tif")%>% 
  as("SpatialPixelsDataFrame") %>% #Turn into dataframe to plot into ggplot
  as.data.frame() %>%
  rename(relief = `Baden500_excess`)

baden_map <- function(visual_data, fill_data, legend)
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
    direction = -1,
    name = legend
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
baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Gesamtbevölkerung")
baden_map(politics_improved, politics_improved$FDP, "FDP %")
