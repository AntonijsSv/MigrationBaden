library(maptools)
library(spdep)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
geo_baden <- read_sf("Boundary_Data/g2g23.shp") #Shape data for Baden commune border
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>%  #List of all Baden communes + some extra data like population
  mutate(GMDNR=`Gmd-Nr.`) #mutate adds a new column to gemeinden_baden, 
#this mutation just makes a new column called GMDNR, it takes and copies the data from the Gmd-Nr.
baden <- left_join(geo_baden,gemeinden_baden, by="GMDNR") %>% #Now you add all the data from gemeinden_baden to geodata
  #you join it by GMDNR meaning rows with the same GMDNR will put together into 1 row, if GMDNR doesnt exist in gemeinden_baden
  #then all the cells with data from gemeinden_baden will be NA as they dont exist
  filter(!is.na(Gemeinde))
# Gemeinde is a column in gemeinden_baden, as all GMDNRs that are not in Baden will not have data for Gemeinde, it will be NA
# By filtering by this column with !is.na, it will check whether it is na, if it is, it will delete that row, if it isnt NA, it will filter it and keep it
# Basically, you take a column that only exists in gemienden_baden which will be NA for communes not in Baden then you filter them out

col.queen.nb <- poly2nb(baden,queen=TRUE) 
col.queen.nb
col.rook.nb <- poly2nb(baden,queen=FALSE)
col.rook.nb
#2 ways to calculate spatial proximities, theyre the same in the case of Baden

baden_sp <- as(baden, 'Spatial')
rook_sf <- as(nb2lines(col.rook.nb, coords = coordinates(baden_sp)), 'sf') #Convert the data into a spatial dataset and add coordinates 
rook_sf <- st_set_crs(rook_sf, st_crs(baden_sp))


ggplot(baden) + #Make a plot with data from baden, nothing else
  geom_sf(fill = 'salmon', color = 'white')+#take the SF data from baden and fill it with salmon colour, outline white
  geom_sf(data=rook_sf,)# new dataset, hence you have to define it, by not defining anything else, it will take on default values

