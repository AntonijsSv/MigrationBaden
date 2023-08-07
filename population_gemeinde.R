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
library(leaflet)
#canton_geo = read_sf("Boundary_Data/g2k23.shp")
#country_geo= read_sf("Boundary_Data/g2l23.shp")

politics <- read_excel("politicalparties.xlsx",col_names = F)
colnames(politics) <- c("Gemeinde","party_num", "party", "percentage") #name the columns
politics <- filter(politics, party_num %in% c(1,2,3,4,31,13))# Filter for only these parties: FDP, CVP, SP, SVP, GLP, GPS

  politics_improved <- data.frame(matrix(ncol = 7, nrow = 0)) #create a new database, empty
  colnames(politics_improved) <- c("Gemeinde", "FDP", "CVP", "SP", "SVP", "GPS", "GLP")
  #for each municipality, give percentage for each party
  for (i in 1:nrow(politics)){ 
    if (i%%6==0) {#Since there are 6 parties, this will happen once for each municipality
      row <- c(politics[i,1])#name of municipality
      for (party in 1:6) {#go thru each party in 1 municipality
        row <- append(row, politics[i+party-6,4])#Add the % for each party to the row
      }
      politics_improved[nrow(politics_improved)+1,] <- row #Add the new row to the new database
    }
  }
  

municipality_geo <- read_sf("Boundary_Data/g2g23.shp") #Shape data for the municipality boundaries
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% #List and Population of each municipality in Baden
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

forest_geo <- read_sf("Forests/Mischungsgrad_Waelder_2015_LV95/Mischungsgrad_LFI_ID164_20_2015_Metadata_LV95.shp")

ggplot()+
  geom_sf(
    data = forest_geo
  )
#Code to create the base map, as the swiss-wide map is too large for github
  #excess <- 2000
  #e_range <- c(min(gemeinden_coords$E_MIN)-excess,max(gemeinden_coords$E_MAX)+excess) 
  #n_range <- c(min(gemeinden_coords$N_MIN)-excess,max(gemeinden_coords$N_MAX)+excess)
  #Create a lat. and long. range for bezirk baden, the excess adds 2000 (LV95 coordinate sys.) to increase the map size

  #map <- raster("Maps/Swiss_500.tif")
  #gemeinden_map <- as(extent(e_range[1]-excess,e_range[2]+excess,n_range[1]-excess, n_range[2]+excess),'SpatialPolygons')
  #Create an empty database with the size of the desired map
  #crs(gemeinden_map) <- crs(map) #Set coordinate system of the new raster -> LV95
  #map500_excess <- crop(map, gemeinden_map) 
  #writeRaster(map500_excess, "Baden500_excess.tif", format="GTIFF")

  
map500 <- raster("Baden500_excess.tif")%>% 
  as("SpatialPixelsDataFrame") %>% #Turn into dataframe to plot into ggplot
  as.data.frame() %>%
  rename(relief = `Baden500_excess`)

baden_map <- function(visual_data, fill_data, legend)
{ 
  ggplot(
  data=visual_data, #Database, where the visual data
  aes(fill=fill_data) #Variable that dictates fill of each municipality
  ) +
  #Map Background
  geom_raster(
    data = map500,
    inherit.aes = FALSE,
    aes(x,y,
      alpha=relief 
      #since fill is already used for the data, alpha values are used to paint the map
      #eventually, either a 2nd fill will be attempted with workarounds, or plot transitioned to leaflet instead of ggplot
    ),
  ) +
  scale_alpha(#How to fill the map
    name = "",
    range = c(0.9,0),
    guide = F
  ) +
  geom_sf( #Create the municiplality Boundaries
    data = gemeinden_coords,
    color = "transparent",
    size = 0.5) +
  
  scale_fill_viridis(#Set a custom fill for the data to be visualised
    option = "magma",
    alpha = 0.4, #make them slightly transparent to see map background
    begin = 0.1,
    end = 0.9,
    direction = -1,
    name = legend
  ) +
  #remove visual clutter
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
baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Gesamtbevölkerung") #visualizes the entire population
baden_map(politics_improved, politics_improved$GPS, "GPS %") #Visualizes these parties:
baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Gesamtbevölkerung")
#GLP, SP, GLP, CVP, FDP, SVP