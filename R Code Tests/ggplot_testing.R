#GGPLOT testing
#make a coloured map
#overlay multiple plots at once
#ultimate goal is to assign hectares to communes, need to figure that out
library(tidyverse) 
library(dplyr)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
#library(cowplot)
library(readr)
library(readxl)
#library(shiny)
#library(magick)
#library(leaflet)
library(ggrepel)
library(xlsx)
library(ggnewscale)
#canton_geo = read_sf("Boundary_Data/g2k23.shp")
#country_geo= read_sf("Boundary_Data/g2l23.shp")

#Open 2 databases, the first one contains coordinate system and population
#each municipality is depicted as 1 hectare in STATPOP, so cannot be used alone
coords_gmde <- read_delim("analysis/statpop/NOLOC/STATPOP2021_NOLOC.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select(E_KOORD,N_KOORD,RELI,GMDE) 
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #give the municipality number column the same name as in coords_gmde

#combines the above datasets and groups them by GMDE,
#for all municipalities outside Baden, this will create NA, which is then filtered out
gmd_data <- left_join(coords_gmde,gemeinden_baden, by="GMDE") %>%
  filter(!is.na(Gemeinde))
#Get the coordinate range of municipalites
#This range is the reason why statpop data is used and not just gemeinden_baden
x_range <- range(gmd_data$E_KOORD)
y_range <- range(gmd_data$N_KOORD)

#Population data per hectare
pop_data <- read_csv("analysis/PopDataperHectare.csv")%>%
  filter(E_KOORD %in% (x_range[1]:x_range[2]), N_KOORD %in% (y_range[1]:y_range[2])) 
#filters data to only show that around Baden



municipality_geo <- read_sf("Boundary_Data/g2g23.shp") #Shape data for the municipality boundaries
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% #List and Population of each municipality in Baden
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

baden_text <- as.data.frame(sf::st_coordinates(sf::st_centroid(gemeinden_coords)))
baden_text$name <- gemeinden_coords$GMDNAME

#Code to create the base map, as the swiss-wide map is too large for github
#excess <- 2000
#e_range <- c(min(gemeinden_coords$E_MIN)-excess,max(gemeinden_coords$E_MAX)+excess) 
#n_range <- c(min(gemeinden_coords$N_MIN)-excess,max(gemeinden_coords$N_MAX)+excess)
#Create a lat. and long. range for bezirk baden, the excess adds 2000 (LV95 coordinate sys.) to increase the map size

#map <- raster("Maps/Swiss_500.tif") %>% 
#  as("SpatialPixelsDataFrame") %>% #Change the file type to convert into a dataframe, ggplot only accepts  data frames
#  as.data.frame()
#gemeinden_map <- as(extent(e_range[1]-excess,e_range[2]+excess,n_range[1]-excess, n_range[2]+excess),'SpatialPolygons')
#Create an empty database with the size of the desired map
#crs(gemeinden_map) <- crs(map) #Set coordinate system of the new raster -> LV95
#map500_excess <- crop(map, gemeinden_map) 
#writeRaster(map500_excess, "Baden500_excess.tif", format="GTIFF")


map500 <- raster("Maps/Baden500_excess.tif")%>% 
  as("SpatialPixelsDataFrame") %>% #Change the file type to convert into a dataframe, ggplot only accepts  data frames
  as.data.frame() %>%
  rename(relief = `Baden500_excess`)

map_colours <- c("white",#unproductive
                 "#CDE6BE",#wooded
                 "red",#Unknown, not on this map
                 "#F6DBA4",#urban areas
                 "#ECA8C8",#cantonal borders
                 "transparent",#cantonal names
                 "red",#Unknown, not on this map
                 "#FFF582",#side roads
                 "#F5AE95",#main roads
                 "orange",#highways
                 "#DD1A19",#train lines
                 "#D2EEFF",#lakes
                 "#0088D0",#River names
                 "#0088D0",#Rivers
                 "#ECA8C8",#National borders
                 "black",#Place names
                 "black",#Airports
                 "black",#Golf courses
                 "black",#Road Outlines
                 "black",#Urban area outlines
                 "black"#City/town dot
                 )
place_names <- rep("transparent",21)
place_names[16] <- "black"
only_map <- ggplot(data = map500, aes(x,y)) + 
  geom_raster(aes(fill=map_colours[relief]))+
  theme_void()+
  scale_fill_identity()

baden_map <- function(visual_data, fill_data, legend)
{ 
  ggplot(
    data=visual_data, #Database, where the visual data
    aes(fill=fill_data) #Variable that dictates fill of each municipality
  ) +
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=map_colours[relief]
          #since fill is already used for the data, alpha values are used to paint the map
          #eventually, either a 2nd fill will be attempted with workarounds, or plot transitioned to leaflet instead of ggplot
      ),
    ) +
    scale_fill_identity() +
    new_scale_fill() +
    #Map Background
    geom_sf( #Create the municiplality Boundaries
      data = gemeinden_coords,
      aes(fill=fill_data),
      color = "transparent",
      size = 0.5) +
    
    scale_fill_viridis(#Set a custom fill for the data to be visualised
      option = "magma",
      alpha = 0.6, #make them slightly transparent to see map background
      begin = 0.1,
      end = 0.9,
      direction = -1,
      name = legend
    ) +
    new_scale_fill() + #Add a new layer to overlay place names above the data
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=place_names[relief]
      ),
    ) +
    scale_fill_identity() +
    
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


baden_hectare <- function(visual_data,e_coord,n_coord, fill_data) 
{
  ggplot(visual_data, aes(x=e_coord,y= n_coord)) + 
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          alpha=relief 
          #since fill is already used for the data, alpha values are used to paint the map
          #eventually, either a 2nd fill will be attempted with workarounds, or plot transitioned to leaflet instead of ggplot
      ),
    ) +
    scale_alpha(
      name = "",
      range = c(0.9,0),
      guide = F 
    ) +
    #visualization of data
    geom_tile(aes(fill=cut(fill_data,
                           c(1,4,7,16,41,121,Inf))
    ),
    ) + 
    #geom_tile(aes(fill=fill_data
    #),
    #) + 
    #by cutting B21BTOT (total population per hectare), you can set a colour to each part, colours mimic those found on the STATPOP website
    #gradients were avoided for the first test, as data wasnt visualsed nicely with gradients
    scale_fill_manual(
      values=c("(1,4]"="#ffffb2",
               "(4,7]"="#fdd976",
               "(7,16]"="#feb243",
               "(16,41]"="#fd8d3c",
               "(41,121]"="#f03b20",
               "(121,Inf]"="#bd0026"), 
      labels=c("1-3","4-6","7-15","16-40","41-120",">120"),
      name="population per ha",
      na.value = "green") +
    #scale_fill_gradient(
    #  name="buildings per ha",
    #  na.value = "green"
    #)+
    #remove visual clutter
    new_scale_fill()+
    
    theme_minimal()+
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

baden_hectare_communes <- function(visual_data_ha,fill_data_ha,visual_data_communes,fill_data_communes,e_coord,n_coord,legend) 
{
  ggplot() + 
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=map_colours[relief]
          #since fill is already used for the data, alpha values are used to paint the map
          #eventually, either a 2nd fill will be attempted with workarounds, or plot transitioned to leaflet instead of ggplot
      ),
    ) +
    scale_fill_identity() +
    new_scale_fill() +
    #visualization of data
    geom_sf( #Create the municiplality Boundaries
      data = gemeinden_coords,
      aes(fill=fill_data_communes),
      color = "transparent",
      size = 0.5) +
    
    scale_fill_viridis(#Set a custom fill for the data to be visualised
      option = "magma",
      alpha = 0.6, #make them slightly transparent to see map background
      begin = 0.1,
      end = 0.9,
      direction = -1,
      name = legend
    ) +
    new_scale_fill()+
    geom_tile(data=visual_data_ha, aes(x=e_coord,y=n_coord, fill=cut(fill_data_ha,
                                                                    c(1,4,7,16,41,121,Inf))
    ),
    ) + 
    #geom_tile(aes(fill=fill_data
    #),
    #) + 
    #by cutting B21BTOT (total population per hectare), you can set a colour to each part, colours mimic those found on the STATPOP website
    #gradients were avoided for the first test, as data wasnt visualsed nicely with gradients
    scale_fill_manual(
      values=c("(1,4]"="#ffffb2",
               "(4,7]"="#fdd976",
               "(7,16]"="#feb243",
               "(16,41]"="#fd8d3c",
               "(41,121]"="#f03b20",
               "(121,Inf]"="#bd0026"), 
      labels=c("1-3","4-6","7-15","16-40","41-120",">120"),
      name="population per ha",
      na.value = "green") +
    
    new_scale_fill() + #Add a new layer to overlay place names above the data
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=place_names[relief]
      ),
    ) +
    scale_fill_identity() +
    theme_minimal()+
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

only_map
baden_hectare(pop_data,pop_data$E_KOORD, pop_data$N_KOORD, pop_data$B21BTOT)

baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Gesamtbevölkerung")

baden_hectare_communes(pop_data,pop_data$B21BTOT,gemeinden_coords, gemeinden_coords$Gesamtbevölkerung,pop_data$E_KOORD, pop_data$N_KOORD,"Gesamtbevölkerung") 


