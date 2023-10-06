# libraries ----
library(tidyverse) 
library(dplyr)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readr)
library(readxl)
library(xlsx)
library(ggnewscale)
library(ggplot2)
#library(shiny)
#library(magick)
#library(leaflet)
#library(cowplot)
#library(ggrepel)
# hectare data filtering ----
coords_gmde <- read_delim("analysis/statpop/NOLOC/STATPOP2021_NOLOC.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::select(E_KOORD,N_KOORD,RELI,GDENR) 
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GDENR=`Gmd-Nr.`) #give the municipality number column the same name as in coords_gmde

baden_commune_names <- gemeinden_baden$Gemeinde #The names of all the communes

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

# Open hectare data files ----

gws <- read_csv("analysis/GWS/GWS_Baden_2021_2012.csv") %>%
  filter(!is.na(E_KOORD), !is.na(N_KOORD))%>%
  mutate(x = E_KOORD, y=N_KOORD)%>%
  relocate(y)%>%
  relocate(x)

statent <- read_csv("analysis/STATENT/STATENT_Baden_2020_2012.csv")%>%
  filter(!is.na(E_KOORD), !is.na(N_KOORD))%>%
  mutate(x = E_KOORD, y=N_KOORD)%>%
  relocate(N_KOORD)%>%
  relocate(E_KOORD)

# shp filtering ----
municipality_geo <- read_sf("Boundary_Data/g2g23.shp") #Shape data for the municipality boundaries
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% #List and Population of each municipality in Baden
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

# sf conversion ----
sf_conversion <- function(df_file,x,y){
#'df_file is the input file, it requires an x and y coordinate column
#'ensure that the x and y columns do not have NAs: df %>% filter(!is.na(column))
#'x and y NEED TO BE STRINGS, should be the name of the column with the coordinates
#'If these columns do not have names, use: colnames(df)[x] <- "E_KOORD"
  sf_file <- st_as_sf(df_file, coords=(c(x,y)))
  #Take popdata and turn it from a basic data frame into a sf file, give it coordinates
  sf_file$centre <- st_centroid(sf_file)
  hectare_size <- 100
  sf_file <- st_buffer(sf_file$centre,0.5*hectare_size,endCapStyle="SQUARE")#%>%
  #  st_simplify()
  #pop_data_sf <- st_make_grid(pop_data_sf,hectare_size)
  #?st_simplify
  sf_file <- st_set_crs(sf_file,st_crs(gemeinden_coords))
  #set the same coordinated system as gemeinden_coords
  sf_file <- st_join(sf_file, gemeinden_coords) %>%
    #Every hectare (point) in pop_data_sf is checked in which commune (mutlipolygon) its located
    #every hectare is assigned to a commune
    filter(!is.na(Gemeinde))
  #remove all points/hectares not inside a commune
  return(sf_file)
}

pop_ha_c <- sf_conversion(pop_data,"E_KOORD","N_KOORD")
pop <- dplyr::select(pop_ha_c,geometry,GMDNAME)

gws_ha <- sf_conversion(gws,"x","y") %>%
  relocate(GMDNAME,)
statent_ha <- sf_conversion(statent,"x","y")

#st_write(pop_ha_communes,"analysis/PopDataperHectare.shp")


pop_ha_communes_df <- st_drop_geometry(pop_ha_c)
gws_ha_c_df <- st_drop_geometry(gws_ha)
statent_ha_c_df <- st_drop_geometry(statent_ha)

write.csv(gws_ha_c_df,"analysis/GWS_21_12_wCommunes.csv")
write.csv(statent_ha_c_df,"analysis/STATENT_21_12_wCommunes.csv")


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


baden_hectare_communes <- function(visual_data_ha,fill_data_ha,
                                   visual_data_communes,fill_data_communes,
                                   legend_ha,legend_commune) 
{
  #'visual data is the data frame with the data
  #'fill data is the the column in the visual data data frame that will be visualized
  #'_ha -> everything with hectares
  #'_communes -> everything with communes
  #'!!!both dataframes need to be converted to sf files -> use sf_conversion function
  #'legend is the title of the legend
  ggplot() + 
    #The map background
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
    
    #visualization of commune data
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
      name = legend_commune
    ) +
    
    new_scale_fill()+
    
    #
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
    scale_fill_viridis(name=legend_ha,
                       begin=0) +
    
    new_scale_fill() + 
    
    #Add a new layer to overlay place names above the data
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=place_names[relief]
      ),
    ) +
    scale_fill_identity() +
    
    #Theme aesthetics
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

baden_hectare_communes(pop_ha_communes,pop_ha_communes$B21BTOT,
                       gemeinden_coords, gemeinden_coords$Gesamtbevölkerung,
                       "population per ha","Gesamtbevölkerung")
baden_hectare_communes(gws_ha_communes,gws_ha_communes$GTOT.x,
                       gemeinden_coords, gemeinden_coords$Gesamtbevölkerung,
                       "buildings per ha","Gesamtbevölkerung")
baden_hectare_communes(gws_ha_c,gws_ha_c$GTOT.x,
                       gemeinden_coords, gemeinden_coords$Gesamtbevölkerung,
                       "buildings per ha","Gesamtbevölkerung")
baden_hectare_communes(statent_ha_c,statent_ha_c$B08T_2020,
                       gemeinden_coords, gemeinden_coords$Gesamtbevölkerung,
                       "workplaces per ha","Gesamtbevölkerung")

 
