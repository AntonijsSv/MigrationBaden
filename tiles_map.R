library(ggplot2) #Access functions to e.g. plot data (ggplot) and read files(readxl/readr)
library(dplyr)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(readr)
library(lintr)

#open Statpop data with all Gemeinden and only select their coordinates and Gemeinde Nr.
#!! Each Gemeinde only has 1 square -> the data is not per hectare
coords_gmde <- read_delim("STATPOP2021_NOLOC.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select(E_KOORD,N_KOORD,GMDE) 

#Open list of all Gemeinden in Bezirk Baden
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #Same name as in pop_data_gmd

#combines the above datasets and groups them by GMDE
#if the Gemeinde isnt in Baden, then you get NA, as that gemeinde will not be in the gemeinden dataset
gmd_data <- left_join(coords_gmde,gemeinden_baden, by="GMDE") %>%
  filter(!is.na(Gemeinde))#Filter out all NA -> all Gemeinden not in Bezirk Baden
#Get the coordinate range all the Gemeinden
x_range <- range(gmd_data$E_KOORD)
y_range <- range(gmd_data$N_KOORD)

#Population data per hectare
pop_data <- read_csv("PopDataperHectare.csv")%>%
  filter(E_KOORD %in% (x_range[1]:x_range[2]), N_KOORD %in% (y_range[1]:y_range[2])) #Only hectares in coordinate range of Bezirk Baden


map500 <- raster("Maps/Baden_500.tif")%>% #Crop the large relief to just the needed size
  as("SpatialPixelsDataFrame") %>% #Turn into dataframe to plot into ggplot
  as.data.frame() %>%
  rename(relief = `Baden_500`)



visual_data <- pop_data
#Create Plot and set x- and y- axis and dataset
baden_tile <- ggplot(visual_data, aes(x=E_KOORD,y= N_KOORD)) + 
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
  #Set the geomertry of each point -> squares/tiles and set how to fill each tile
  geom_tile(aes(fill=cut(B21BTOT,
                         c(1,4,7,16,41,121,Inf)
                         )
                ),
            ) + 
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
  #remove all axis text and titles
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

baden_tile

