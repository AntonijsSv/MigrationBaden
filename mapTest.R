library(rstudioapi)
library(tidyverse) 
library(dplyr)
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)
library(readr)
library(readxl)
#canton_geo = read_sf("Boundary_Data/g2k23.shp")
#lake_geo = read_sf("Boundary_Data/g2s23.shp")
#country_geo= read_sf("Boundary_Data/g2l23.shp")

municipality_geo = read_sf("Boundary_Data/g2g23.shp")
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`)

gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>%
  filter(!is.na(Gemeinde))

pop_data <- read_delim("STATPOP2021_NOLOC.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(data=pop_data)+
  geom_sf(data = gemeinden_coords,
          fill=Gesamtbevölkerung,
          color = "navy",
          size = 0.5)+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
  )
