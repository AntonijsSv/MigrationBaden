
# Libraries ----
library(tidyverse)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(patchwork)

#Files ----
sf_conversion <- function(df_file,x,y){
  #'df_file is the input file, it requires an x and y coordinate column
  #'ensure that the x and y columns do not have NAs: df %>% filter(!is.na(column))
  #'x and y NEED TO BE STRINGS, should be the name of the column with the coordinates
  #'If these columns do not have names, use: colnames(df)[x] <- "E_KOORD"
  #'commune_geo should be an sf file of Baden -> boundaries
  sf_file <- st_as_sf(df_file, coords=(c(x,y)))
  #Take popdata and turn it from a basic data frame into a sf file, give it coordinates
  sf_file$centre <- st_centroid(sf_file)
  hectare_size <- 100
  sf_file <- st_buffer(sf_file$centre,0.5*hectare_size,endCapStyle="SQUARE")#%>%
  #  st_simplify()
  #pop_data_sf <- st_make_grid(pop_data_sf,hectare_size)
  #?st_simplify
  sf_file <- st_set_crs(sf_file,st_crs(commune_geo))
  #set the same coordinated system as gemeinden_coords
  sf_file <- st_join(sf_file, commune_geo)
    #Every hectare (point) in pop_data_sf is checked in which commune (mutlipolygon) its located
    #every hectare is assigned to a commune
  sf_file <- filter(sf_file,!is.na("Gemeinde"))
  #remove all points/hectares not inside a commune
  return(sf_file)
}

municipality_geo <- read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`)
#All swiss boundary data

commune_general_info <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))
#Filter the data to just Baden

commune_geo <- dplyr::select(commune_general_info,c(Gemeinde,GMDNR,geometry))
commune_geo$Gemeinde <- gsub("\\(AG)|", "",commune_geo$Gemeinde)%>%
  str_trim()
#get just the most important data

population <- read_xlsx("analysis/population_baden.xlsx")
pop <- dplyr::select(population,"GMDE","population" = "2022")

population_geo <- read_csv("analysis/STATPOP_21_11_wCommunes.csv")%>%
  dplyr::select(-...1)%>%
  mutate(x = E_KOORD, y = N_KOORD)

pop_geo <- sf_conversion(population_geo,"x","y")%>%
  dplyr::select(-Gemeinde.y)
colnames(pop_geo)[1] <- "Gemeinde"
commune_geo <- left_join(commune_geo,pop, by= join_by(GMDNR==GMDE) )

communes <- gemeinden_baden$Gemeinde

# ggplot map ----
map500 <- raster("Maps/Baden500_excess.tif")%>% 
  as("SpatialPixelsDataFrame") %>% #Change the file type to convert into a dataframe, ggplot only accepts  data frames
  as.data.frame() %>%
  rename(relief = `Baden500_excess`)

map_colours <- c("white",# Generic land
                 "#CDE6BE",# wooded
                 "red",# Unknown, not on this map
                 "#F6DBA4",# urban areas
                 "#ECA8C8",# cantonal borders
                 "transparent",# cantonal names
                 "red",# Unknown, not on this map
                 "#FFF582",# side roads
                 "#F5AE95",# main roads
                 "orange",# highways
                 "#DD1A19",# train lines
                 "#D2EEFF",# lakes
                 "#0088D0",# River names
                 "#0088D0",# Rivers
                 "#ECA8C8",# National borders
                 "black",# Place names
                 "black",# Airports
                 "black",# Golf courses
                 "black",# Road Outlines
                 "black",# Urban area outlines
                 "black"# Place dot 
)
place_names <- rep("transparent",21)
place_names[16] <- "black"


baden_commune_map <- function(visual_data,fill_data,legend) 
{
  #'visual data is the data frame with the data
  #'fill data is the the column in the visual data data frame that will be visualized
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
      data = visual_data,
      aes(fill=fill_data),
      color = "black",
      size = 1) +
    
    scale_fill_viridis(#Set a custom fill for the data to be visualised
      option = "magma",
      alpha = 0.6, #make them slightly transparent to see map background
      begin = 0.1,
      end = 0.9,
      direction = -1,
      name = legend
    ) +
    
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


coefficients <- matrix(0, nrow = length(communes), ncol= 5)
rownames (coefficients) <- communes
colnames(coefficients) <- c("Houseprices",
                            "Rentprices",
                            "Public Transport",
                            "Job Opportunities",
                            "Services")

pop_v <- as.matrix(population$"2022"[-1])

move_out <- matrix(0, nrow = length(communes), ncol = 6)
rownames(move_out) <- communes
colnames(move_out) <- c("General %","0-1km", "1-5km", "5-10km","10-50km","50+km")

#Create Emigration Probability Matrix ----
commune_type <- matrix(0,nrow=length(communes),ncol=2)
commune_type[,1]<- communes
commune_type[,2] <- c(1,4,2,5,5,2,3,5,2,3,4,5,5,2,5,4,2,5,3,5,2,2,2,5,5,3)
move_out_percent <- data.frame("urban"= c(.097,.305,.344,.114,.175,0.062),
                               "small_urban" = c(.093,.344,.29,.102,.198,.067),
                               "peri-urban" = c(.078,.258,.244,.189,.25,.059),
                               "rural-centre" = c(.091,.37,.213,.099,.235,.082),
                               "rural" = c(.078,.268,.239,.167,.27,.054))
#Data collected from 
#https://www.swissstats.bfs.admin.ch/collection/ch.admin.bfs.swissstat.en.issue201420182000/article/issue201420182000-10
for (commune in 1:length(communes)) {
  type <- as.numeric(commune_type[commune,2])
  for (i in 1:ncol(move_out)) {
    move_out[commune,i] <- move_out_percent[i,type]
  }
}

# Emigration Simulation ----
#Input is the emigrating population
#2000 = 2km
baden_ha <- 
near_Baden <- st_is_within_distance(commune_geo$geometry[1],commune_geo,dist=2000)[[1]]
baden_commune_map(commune_geo[near_Baden,],commune_geo$population[near_Baden],"population")



