library(maptools)
library(spdep)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
geo_baden <- read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`) 
baden <- left_join(geo_baden,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

col.queen.nb <- poly2nb(baden,queen=TRUE)
col.queen.nb
col.rook.nb <- poly2nb(baden,queen=FALSE)
col.rook.nb

baden_sp <- as(baden, 'Spatial')
rook_sf <- as(nb2lines(col.rook.nb, coords = coordinates(baden_sp)), 'sf')
rook_sf <- st_set_crs(rook_sf, st_crs(baden_sp))


ggplot(baden) +
  geom_sf(fill = 'salmon', color = 'white')+
  geom_sf(data=rook_sf,)

