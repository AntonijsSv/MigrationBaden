
# Libraries ----
library(tidyverse)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(patchwork)

#Files ----
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`)

population <- read_xlsx("analysis/population_baden.xlsx")

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

move_out <- matrix(0, nrow = length(communes), ncol = 5)
rownames(move_out) <- communes
colnames(move_out) <- c("0-1km", "1-5km", "5-10km","10-50km","50+km")

#Create Emigration Probability Matrix ----
commune_type <- matrix(0,nrow=length(communes),ncol=2)
commune_type[,1]<- communes
commune_type[,2] <- c(1,4,2,5,5,2,3,5,2,3,4,5,5,2,5,4,2,5,3,5,2,2,2,5,5,3)
move_out_percent <- data.frame("urban"= c(.305,.344,.114,.175,0.062),
                               "small_urban" = c(.344,.29,.102,.198,.067),
                               "peri-urban" = c(.258,.244,.189,.25,.059),
                               "rural-centre" = c(.37,.213,.099,.235,.082),
                               "rural" = c(.268,.239,.167,.27,.054))
#Data collected from 
#https://www.swissstats.bfs.admin.ch/collection/ch.admin.bfs.swissstat.en.issue201420182000/article/issue201420182000-10
for (commune in 1:length(communes)) {
  type <- as.numeric(commune_type[commune,2])
  for (i in 1:5) {
    move_out[commune,i] <- move_out_percent[i,type]
  }
}

# Emigration Simulation ----
#Input is the emigrating population

