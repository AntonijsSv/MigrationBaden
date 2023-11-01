#Program to plot data

# Libraries ----
library(tidyverse)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(patchwork)
library(ggnewscale)

# General Files ----
#Shape data for the municipality boundaries
municipality_geo <- read_sf("Boundary_Data/g2g23.shp")

#List and Population of each municipality in Baden in 2021
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
commune_general_info <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

#purely geographical information for each commune
commune_geo <- dplyr::select(commune_general_info,c(Gemeinde,GMDNR,geometry))
commune_geo$Gemeinde <- gsub("\\(AG)|", "",commune_geo$Gemeinde)%>%
  str_trim()
#remove (AG) suffix to some communes 
commune <- commune_general_info$GMDNAME

# Population files----
#purely the population of each commune from 2022 to 2011
population <- read_xlsx("analysis/population_baden.xlsx")%>%
  dplyr::select(-GMDE)

pop <- t(population)
colnames(pop) <- pop[1,]
pop <- as.tibble(pop[-1,])
colnames(pop)[2:ncol(pop)] <- paste0("pop_",colnames(pop)[2:ncol(pop)])
pop$Years <- str_trim(pop$Years)

# Factor Files ----
gws <- read_csv("analysis/GWS_Communes.csv") %>%
  dplyr::select(-"...1")
gws_geo <- left_join(commune_geo,gws,by=join_by(Gemeinde==Communes))
statent <- read_xlsx("analysis/STATENT_Communes.xlsx")
statent_geo <- left_join(commune_geo,statent,by=join_by(Gemeinde))

ov <- read_csv("OeV/OV_Baden_communes_2023_2013.csv")

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

# ggplot ----
double_analysis <- function(visual_data,
                     x,
                     y,
                     commune,
                     commune_lab,
                     data_title,
                     x_lab,
                     y_lab) {
  pop_plot <- ggplot() +
    geom_point(data=pop,aes(x=Years,y=commune))+
    labs(title = "Population over time") +
    xlab("Time") +
    ylab(commune_lab)
  
  data_plot <- ggplot(data=visual_data)+
    geom_point(aes(x=x,y=y)) +
    labs(title = data_title) +
    xlab(x_lab) +
    ylab(y_lab)
  
  pop_plot + data_plot
}

analysis_colour <- function(colour_df,
                          commune_name,
                          title,
                          x_lab,
                          y_lab,
                          colour_lab) {
  pop <- dplyr::select(pop,Years,commune = commune_name)
  pop$Years <- as.numeric(pop$Years)
  data <- dplyr::select(colour_df, Years, colour = commune_name)
  data$Years <- as.numeric(data$Years)
  
  df <- left_join(pop,data, by = "Years")
  
  ggplot(data = df, aes(x = Years, y = commune)) +
    geom_point(aes(colour = colour)) +
    scale_colour_viridis(name = colour_lab) +
    labs(title = title) +
    xlab(x_lab) +
    ylab(y_lab)
}
# Data Analysis ----
dataset <- function(data,value) {
  rownames(data) <- data[,1]
  data <-  dplyr::select(data,contains(value)) #only select values to plot
  colnames(data) <- gsub("(.*?)_(\\d+)", "\\2", colnames(data)) #Finds the year in the colnames and extracts the year
  data <- t(data) 
  data <- cbind(rownames(data),data.frame(data, row.names = NULL))
  colnames(data)[1] <- "Years"
  return(data)
  
}

gws_tot <- (dataset(as.data.frame(gws),"GTOT"))
statent_tot <- (dataset(as.data.frame(statent),"B08T"))

colnames(gws_tot)[2:ncol(gws_tot)] <- paste0("gws_",colnames(gws_tot)[2:ncol(gws_tot)])
colnames(statent_tot)[2:ncol(statent_tot)] <- paste0("ent_",colnames(statent_tot)[2:ncol(statent_tot)])

gws_ent <- full_join(gws_tot,statent_tot, by = "Years")

factor_pop <- left_join(gws_ent,pop, by = "Years")

lm_gws <- lm(pop_Baden~Baden, data=gws_pop)
summary(lm_gws)

statent_pop <-left_join(statent_tot,pop, by = "Years") 
lm_ent <- lm(pop_Baden~Baden, data=statent_pop)
summary(lm_ent)

lm_factors<- lm(pop_Baden~ent_Baden, data = factor_pop)
summary(lm_factors)

plot(statent_pop$Baden,statent_pop$pop_Baden,pch = 16, col = "#009999")
plot(lm_ent$residuals,pch = 16, col = "#009999")
plot(cooks.distance(lm_ent),pch = 16, col = "#009999")
# Plots ----
analysis_colour(gws_tot,
              "Baden",
              "Population in relation to amount of Buildings",
              "Time",
              "Population",
              "Amount of Buildings")

double_analysis(gws_tot,gws_tot$Years,gws_tot$Baden,
              pop$Baden,"Population Baden",
              "Gws Baden", "Time", "Amount of Buildings")

double_analysis(statent_tot,statent_tot$Years,statent_tot$Baden,
         pop$Baden, "Population Baden",
         "Statent Baden", "Time", "Amount of Businesses")

analysis_colour(statent_tot,
                "Baden",
                "Population in relation to amount of Businesses",
                "Time",
                "Population",
                "Amount of Businesses")

plot(gws_tot$Baden,pop$Baden[2:10])

baden_commune_map(statent_geo,statent_geo$B08T_2021,"statent")
