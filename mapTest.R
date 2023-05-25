library(rstudioapi)
library(tidyverse) 
library(dplyr)
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(rmarkdown)
library(readr)
library(readxl)
#canton_geo = read_sf("Boundary_Data/g2k23.shp")

#country_geo= read_sf("Boundary_Data/g2l23.shp")

municipality_geo <- read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`,gemeinde_pop=`Gesamtbevölkerung`)

gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>%
  filter(!is.na(Gemeinde))
  e_range <- c(min(gemeinden_coords$E_MIN)-1000,max(gemeinden_coords$E_MAX)+1000)
  n_range <- c(min(gemeinden_coords$N_MIN)-1000,max(gemeinden_coords$N_MAX)+1000)

  
map <- raster("Maps/baden_relief1.tif")%>%
  # hide relief outside of Switzerland by masking with country borders
  mask(gemeinden_coords) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = `X02.relief.ascii`)

  
ggplot(data=gemeinden_coords,aes(fill=Gesamtbevölkerung))+
  geom_sf(data = gemeinden_coords,
          color = "transparent",
          size = 0.5)+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
  ) +
  scale_fill_viridis(
    option = "magma",
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    direction = -1
  )+
  geom_raster(data=map_df, aes(x=x, y=y, fill=baden_relief1))


