# libraries ----
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
library(ggplot2)
# hectare data filtering ----
coords_gmde <- read_delim("analysis/statpop/NOLOC/STATPOP2021_NOLOC.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select(E_KOORD,N_KOORD,RELI,GDENR) 
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GDENR=`Gmd-Nr.`) #give the municipality number column the same name as in coords_gmde

#combines the above datasets and groups them by GMDE,
#for all municipalities outside Baden, this will create NA, which is then filtered out
gmd_data <- left_join(coords_gmde,gemeinden_baden, by="GDENR") %>%
  filter(!is.na(Gemeinde))
#Get the coordinate range of municipalites
#This range is the reason why statpop data is used and not just gemeinden_baden
x_range <- range(gmd_data$E_KOORD)
y_range <- range(gmd_data$N_KOORD)

excess <- 2000
x_range[1] <- x_range[1] - excess
x_range[2] <- x_range[2] + excess
y_range[1] <- y_range[1] - excess
y_range[2] <- y_range[2] + excess

#Population data per hectare
pop_data <- read_csv("analysis/PopDataperHectare.csv")%>%
  filter(E_KOORD %in% (x_range[1]:x_range[2]), N_KOORD %in% (y_range[1]:y_range[2])) 

#filters data to only show that around Baden
# shp filtering ----
municipality_geo <- read_sf("Boundary_Data/g2g23.shp") #Shape data for the municipality boundaries
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% #List and Population of each municipality in Baden
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

# sf conversion ----
pop_data_sf <- st_as_sf(pop_data, coords=(c("E_KOORD","N_KOORD")))
#Take popdata and turn it from a basic data frame into a sf file, give it coordinates
pop_data_sf$centre <- st_centroid(pop_data_sf)
hectare_size <- 100
pop_data_sf <- st_buffer(pop_data_sf$centre,0.5*hectare_size,endCapStyle="SQUARE")#%>%
#  st_simplify()
#pop_data_sf <- st_make_grid(pop_data_sf,hectare_size)
#?st_simplify
pop_data_sf <- st_set_crs(pop_data_sf,st_crs(gemeinden_coords))
#set the same coordinated system as gemeinden_coords
ha_communes <- st_join(pop_data_sf, gemeinden_coords) %>%
#Every hectare (point) in pop_data_sf is checked in which commune (mutlipolygon) its located
#every hectare is assigned to a commune
  filter(!is.na(Gemeinde))
#remove all points/hectares not inside a commune
# ggplot ----

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
    geom_sf(data=visual_data_ha, 
            aes(fill=fill_data_ha),
            colour = "transparent",
            size = 1,
    ) + 
    #geom_tile(aes(fill=fill_data
    #),
    #) + 
    #by cutting B21BTOT (total population per hectare), you can set a colour to each part, colours mimic those found on the STATPOP website
    #gradients were avoided for the first test, as data wasnt visualsed nicely with gradients
    scale_fill_viridis(name="population per ha",
                       begin=0) +
    
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

baden_hectare_communes(ha_communes,ha_communes$B21BTOT,gemeinden_coords, gemeinden_coords$Gesamtbevölkerung,ha_communes$E_CNTR, ha_communes$N_CNTR,"Gesamtbevölkerung")

 
