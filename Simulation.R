
# Libraries ----
library(tidyverse)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(patchwork)
library(ggnewscale)
library(ggarchery)

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
ha_geo <- dplyr::select(pop_geo,Gemeinde,E_KOORD,N_KOORD,geometry,B21BTOT)

commune_geo <- left_join(commune_geo,pop, by= join_by(GMDNR==GMDE) )

communes <- gemeinden_baden$Gemeinde
communes <- gsub("\\(AG)|", "",communes)%>%
  str_trim()
communes<-communes[-5]
communes <- c(communes,"Ehrendingen")
pop_v <- as.data.frame(population$"2021"[-1])
colnames(pop_v) <- "old_pop"
pop_v$new_pop <- pop_v$old_pop


pop_analysis <- function(pop) {
    pop <- mutate(pop,pop_change= pop$new_pop-pop$old_pop)
    #pop[[pop_change]] <- pop[[new_pop]]-pop$old_pop
    pop <- mutate(pop,pop_percentage=pop$new_pop/pop$old_pop)
    #pop[[pop_percentage]] <- pop[[new_pop]]/pop$old_pop
    pop <- mutate(pop,net_migration_percent=pop$pop_percentage-1-0.01050109*0.8)
    #pop$net_migration_percent <- pop[[pop_percentage]]-1-0.01050109*0.8
    #net population growth in the entire district*the amount of that originating from birth/death
    #pop[[net_migration]] <- pop$old_pop*pop[[net_migration_percent]]
    pop <- mutate(pop,net_migration=pop$old_pop*pop$net_migration_percent)
  
  
  rownames(pop) <- communes
  return (pop)
}
pop_v <- pop_analysis(pop_v)

fpop <- filter(factor_pop, Years == 2021)
fpop_22 <- filter(factor_pop, Years == 2022) %>%
  dplyr::select(starts_with("pop"))

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


baden_commune_arrow <- function(point1,point2,visual_data,fill_data,legend) 
{
  #'visual data is the data frame with the data
  #'fill data is the the column in the visual data data frame that will be visualized
  #'legend is the title of the legend
  coords <- data.frame(x1=st_coordinates(point1)[1],
                       x2=st_coordinates(point2)[1],
                       y1=st_coordinates(point1)[2],
                       y2=st_coordinates(point2)[2])
  
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
    geom_segment(data=coords, 
                 aes(x=x1,y=y1,xend=x2,yend=y2),
                 arrow = arrow(angle=30,length=unit(0.1,"cm"),ends="last",type="closed"),
                 colour = "red",
                 linewidth = 1,
                 #transition_manual(time)
    )+
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

baden_hectare_map <- function(visual_data,fill_data,legend) 
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
    #
    geom_sf(data=visual_data, 
            aes(fill=fill_data,
                inherit.aes=FALSE),
            colour = "transparent",
            size = 1,
    ) + 
    #geom_tile(aes(fill=fill_data
    #),
    #) + 
    #by cutting B21BTOT (total population per hectare), you can set a colour to each part, colours mimic those found on the STATPOP website
    #gradients were avoided for the first test, as data wasnt visualsed nicely with gradients
    scale_fill_viridis(name=legend,
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
baden_hectare_arrow <- function(ha1,ha2,visual_data,fill_data,legend)
{
  #'visual data is the data frame with the data
  #'fill data is the the column in the visual data data frame that will be visualized
  #'_ha -> everything with hectares
  #'_communes -> everything with communes
  #'!!!both dataframes need to be converted to sf files -> use sf_conversion function
  #'legend is the title of the legend
  coords <- data.frame(x1=ha1$E_KOORD,
                       x2=seq(ha1$E_KOORD,ha2$E_KOORD, length=30),
                       y1=ha1$N_KOORD,
                       y2=seq(ha1$N_KOORD,ha2$N_KOORD, length=30),
                       time = 1:30)
  view(coords)
  str(coords)
  map <- ggplot() + 
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
    #
    geom_sf(data=visual_data, 
            aes(fill=fill_data),
            colour = "transparent",
            size = 1,
    ) + 
    #geom_tile(aes(fill=fill_data
    #),
    #) + 
    #by cutting B21BTOT (total population per hectare), you can set a colour to each part, colours mimic those found on the STATPOP website
    #gradients were avoided for the first test, as data wasnt visualsed nicely with gradients
    scale_fill_viridis(name=legend,
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
    geom_segment(data=coords, 
                 aes(x=x1,y=y1,xend=x2,yend=y2),
                 arrow = arrow(angle=30,length=unit(0.1,"cm"),ends="last",type="closed"),
                 colour = "red",
                 linewidth = 1,
                 transition_manual(time)
                 )+
    #Theme aesthetics
    theme_minimal()+
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
    )
    return(map)
}
# Slider Input ----
slider_demo <- data.frame(
  Name = c("Commune","Health services:", "House/Rental Prices:", "Job Opportunities:","Education"),
  Value = c("Baden",1,2,1,1),
  stringsAsFactors = FALSE)
# Coefficients ----

coef_Baden <- data.frame(coefficient=c("Intercept","rent_baden","edu_baden","ent_baden","health_baden"),
                         value = c(18722.953506, 11.350421, 4.829288, -1.240632, 4.364191))
coef_Bellikon <- data.frame(coefficient=c("Intercept","rent_bellikon","edu_bellikon","ent_bellikon","health_bellikon"),
                            value = c(1473.5045931, 0.3991652, 2.3630931, 0.3799376, -1.2387387))
coef_Bergdietikon <- data.frame(coefficient=c("Intercept","rent_bergdietikon","edu_bergdietikon","health_bergdietikon","ent_bergdietikon","ent_bellikon"),
                            value = c(2605.392354, 3.194327, -13.046932, 13.687470, 1.937724, -6.995531))
coef_Birmenstorf <- data.frame(coefficient=c("Intercept","house_birmenstorf","ent_birmenstorf","health_birmenstorf","edu_birmenstorf"),
                         value = c(-2509.1699083, 0.1036378, 29.0945234, -17.4955843, -6.7901265))
coef_Ehrendingen <- data.frame(coefficient=c("Intercept","rent_ehrendingen","health_ehrendingen","edu_ehrendingen","ent_ehrendingen","health_ennetbaden"),
                         value = c(4803.4280544, 2.3406851, -37.5065624, 8.3782970, 0.3075315, 14.5713066))
coef_Ennetbaden <- data.frame(coefficient=c("Intercept","rent_ennetbaden","edu_baden","health_ennetbaden","ent_ennetbaden","edu_ennetbaden"),
                         value = c(1976.895519, 2.548093, 4.237226, -5.628923, 3.327639, 3.178744 ))
coef_Fislisbach <- data.frame(coefficient=c("Intercept","house_fislisbach","health_fislisbach","edu_fislisbach","ent_fislisbach"),
                         value = c(3824.6934918,0.1686995,-20.7352791,6.7736144,4.4623245))
coef_Freienwil <- data.frame(coefficient=c("Intercept","house_freienwil","edu_freienwil","ent_freienwil","health_freienwil"),
                         value = c(390.65135613, 0.09613579, 0, -1.64552364, 28.73733984))
coef_Gebenstorf <- data.frame(coefficient=c("Intercept","house_gebenstorf","ent_gebenstorf","health_gebenstorf","edu_gebenstorf"),
                             value = c(1832.8666469, 0.4895858, 1.0662318, -5.1684264, 6.3912439))
coef_Killwangen <- data.frame(coefficient=c("Intercept","house_killwangen","ent_killwangen","edu_killwangen","health_killwangen"),
                         value = c(878.63922189, 0.08506498, 6.05574741, -18.20455843, -10.33643870))
coef_Künten <- data.frame(coefficient=c("Intercept","house_künten","health_künten","edu_künten","ent_künten"),
                         value = c(928.60548084, 0.13144222, 8.47340091, -5.01405734, -0.01651525))
coef_Mägenwil <- data.frame(coefficient=c("Intercept","rent_mägenwil","ent_mägenwil","edu_mellingen","edu_mägenwil","health_mägenwil"),
                         value = c(1582.738044, 1.702261, 1.333401, 3.911492, 2.778947, 6.970707))
coef_Mellingen <- data.frame(coefficient=c("Intercept","rent_mellingen","health_mellingen","edu_mellingen","ent_mellingen","ent_fislisbach"),
                         value = c(274.7192201, 12.8040883, 0.5836928, -48.0007417, 15.0362766, 0.4539605))
coef_Neuenhof <- data.frame(coefficient=c("Intercept","house_neuenhof","health_neuenhof","edu_neuenhof","ent_neuenhof"),
                         value = c(4320.1430750, 0.4630323, -7.9489663, -22.6455862, 5.1012512))
coef_Niederrohrdorf <- data.frame(coefficient=c("Intercept","house_niederrohrdorf","edu_oberrohrdorf","ent_niederrohrdorf","edu_niederrohrdorf","health_niederrohrdorf"),
                         value = c(1877.5472727, 0.4474443, -43.3997678, 1.8055217, -2.6150562, -16.6376079 ))
coef_Oberrohrdorf <- data.frame(coefficient=c("Intercept","house_oberrohrdorf","edu_oberrohrdorf","ent_oberrohrdorf","health_oberrohrdorf"),
                         value = c(3084.7303926, 0.1529742, -0.9588283, 6.1889287, 3.2997615))
coef_Obersiggenthal <- data.frame(coefficient=c("Intercept","house_obersiggenthal","health_obersiggenthal","ent_obersiggenthal","edu_obersiggenthal"),
                                  value = c(4.770200e+03, 9.132275e-02, 2.717372e+01, 5.456018e+00,1.199151e+01))
coef_Remetschwil <- data.frame(coefficient=c("Intercept","rent_remetschwil","edu_remetschwil","health_remetschwil","ent_remetschwil"),
                                  value = c(2632.5060709, 0.1523921, 0.2619645, 20.6831222, -5.4645576))
coef_Spreitenbach <- data.frame(coefficient=c("Intercept","rent_spreitenbach","health_spreitenbach","ent_spreitenbach","edu_spreitenbach"),
                                  value = c(10131.000818, 11.699141, 56.819847, -3.912515, 25.383187))
coef_Turgi <- data.frame(coefficient=c("Intercept","rent_turgi","ent_turgi","health_turgi","edu_turgi"),
                                  value = c(2376.939820, 0.032677, 2.044775, 2.796493, 3.028180))
coef_Stetten <- data.frame(coefficient=c("Intercept","rent_stetten","health_stetten","edu_stetten","ent_stetten"),
                         value = c(2001.469, 7.369, -2.398, 8.594, -3.416))
coef_Untersiggenthal <- data.frame(coefficient=c("Intercept","house_untersiggenthal","ent_untersiggenthal","health_untersiggenthal","edu_untersiggenthal"),
                         value = c(4620.33048275, 0.09753149, 6.37650787, 14.58098721, -8.36437175 ))
coef_Wettingen <- data.frame(coefficient=c("Intercept","rent_wettingen","edu_wettingen","health_wettingen","ent_wettingen"),
                         value = c(12772.693753, 4.406298, 35.816860, 6.977471, 2.944182))
coef_Wohlenschwil <- data.frame(coefficient=c("Intercept","house_wohlenschwil","health_wohlenschwil","edu_wohlenschwil","ent_wohlenschwil"),
                         value = c(1113.81743263, 0.09659737, -2.93676094, -7.05608405, -0.65940067))
coef_Würenlingen <- data.frame(coefficient=c("Intercept","rent_würenlingen","health_würenlingen","edu_würenlingen","ent_würenlingen","ent_Untersiggenthal"),
                         value = c(2650.178783, 10.211383, 3.371687, -12.522055, -1.714890, 6.819109))
coef_Würenlos <- data.frame(coefficient=c("Intercept","house_würenlos","health_würenlos","edu_würenlos","ent_würenlos","health_Neuenhof"),
                         value = c(2544.8913199, 0.5344122, 0.7655624, -9.5892315, 3.3145121, -40.5820143))


coefficients <- paste0("coef_",communes)


# Emigration Matrix ----
move_out <- matrix(0, nrow = length(communes), ncol = 5)
rownames(move_out) <- communes
colnames(move_out) <- c("General %","0-1km", "1-5km", "5-10km","10+km")


commune_type <- matrix(0,nrow=length(communes),ncol=5)
commune_type[,1]<- communes
commune_type[,2] <- c(1,4,2,5,5,2,3,5,2,3,4,5,5,2,5,4,2,5,3,5,2,2,2,5,5,3)
move_out_percent <- data.frame("urban"= c(.097,.305,.344,.114,.237),
                               "small_urban" = c(.093,.344,.29,.102,.265),
                               "peri-urban" = c(.078,.258,.244,.189,.309),
                               "rural-centre" = c(.091,.37,.213,.099,.317),
                               "rural" = c(.078,.268,.239,.167,.324))
#Data collected from 
#https://www.swissstats.bfs.admin.ch/collection/ch.admin.bfs.swissstat.en.issue201420182000/article/issue201420182000-10
for (commune in 1:length(communes)) {
  type <- as.numeric(commune_type[commune,2])
  for (i in 1:ncol(move_out)) {
    move_out[commune,i] <- move_out_percent[i,type]
  }
}

#Data collected from 
#https://www.swissstats.bfs.admin.ch/collection/ch.admin.bfs.swissstat.en.issue201420182000/article/issue201420182000-10
for (commune in 1:length(communes)) {
  type <- as.numeric(commune_type[commune,2])
  for (i in 1:ncol(move_out)) {
    move_out[commune,i] <- move_out_percent[i,type]
  }
}

#Emigration Calculation ----


emigration_ha <- function(commune) {
  #Finding Start and End
  emigration_distance <- sample(1:5,size=1,prob=move_out[1,2:6])#Gives a random distance the people will move out
  ha_in_commune <- filter(ha_geo,Gemeinde == commune)
  n_ha <- sample(1:nrow(ha_in_commune),1)
  ha <- st_centroid(ha_in_commune[n_ha,])
  near_ha_lower_lim <- st_is_within_distance(
    ha$geometry,ha_geo,
    dist=lower_lim[emigration_distance])[[1]]
  ha_distance <- st_is_within_distance(
    ha$geometry,ha_geo,
    dist=upper_lim[emigration_distance])[[1]]%>%
    setdiff(near_ha_lower_lim)
  
  ha_within_distance <- ha_geo[ha_distance,]%>%
    filter(Gemeinde != commune)
  
  ha_goal <- ha_within_distance[sample(1:nrow(ha_within_distance),1),]
  
  ha_goal <- st_centroid(ha_goal)
  
  #Plot
  p <- baden_commune_arrow(ha,ha_goal,commune_geo,commune_geo$population,"Pop")
  return(p)
}
move_in_commune <- data.frame(immigration = rep(0,26))
move_in_commune <- t(move_in_commune)
colnames(move_in_commune) <- communes

emigration_1commune <- function(commune, population_change) {

  
  n <- which(communes == commune)
  emigration_distance <- population_change*move_out[n,2:6]#Gives a random distance the people will move out
  commune_start <- st_centroid(filter(commune_geo, Gemeinde == commune))
  ha_in_commune <- filter(ha_geo,Gemeinde == commune)
  n_ha <- sample(1:nrow(ha_in_commune),population_change, replace=TRUE)
  ha <- st_centroid(ha_in_commune[n_ha,])
  pop_change_10 <- as.integer(population_change/10)
  print(pop_change_10)
  
  for (i in 1:pop_change_10) {
    print("start")
    distance <- emigration_distance[i]
    print(distance)
    move_in_commune[1,] <- 0
    
    near_ha_lower_lim <- st_is_within_distance(
      ha$geometry,ha_geo,
      dist=lower_lim[distance])[[1]]
    print(near_ha_lower_lim)
    
    ha_distance <- st_is_within_distance(
      ha$geometry,ha_geo,
      dist=upper_lim[distance])[[1]]%>%
      setdiff(near_ha_lower_lim)
    print(ha_distance[])
    ha_within_distance <- ha_geo[ha_distance,]%>%
      filter(Gemeinde != commune)
    ha_goal <- ha_within_distance[sample(1:nrow(ha_within_distance),1,replace=TRUE),]
    c <- st_drop_geometry(dplyr::select(ha_goal, Gemeinde))
    
    print(c)
    print(i)
  }
  
  #ha_goal <- st_centroid(ha_goal)
  #commune_goal <- st_drop_geometry(ha_goal[1,1])
  #commune_destination <- st_centroid(filter(commune_geo, Gemeinde == as.character(commune_goal)))
  
  #Plot
  p <- baden_commune_arrow(commune_start,commune_destination,commune_geo,commune_geo$population,"Pop")
  return(p)
}

#emigration_1commune("Untersiggenthal",1000)

emigration_distance <- function(df) {
  #' first column is communes and 2nd is an emigration column
  commune = "Baden"
  pop_emigrate <- df[commune,2]
  emigrate_distance <- pop_emigrate*move_out[commune,2:5]
  commune_start <- filter(commune_geo, Gemeinde == commune)
  ha_in_commune <- st_within(ha_geo$geometry,commune_start$geometry)[[1]]
  ha_in_commune <- ha_geo[ha_in_commune,]
  print("1km")
  print(st_centroid(commune_start))
  
  
  distance_1km_communes <- st_is_within_distance(commune_start,commune_geo,dist=1000)[[1]]%>%
    setdiff(ha_in_commune)
  communes_1km <- commune_geo[distance_1km_communes,]
  #print(baden_commune_map(communes_1km$geometry,communes_1km$population,"pop"))
  communes_name_1k <- unique(communes_1km$Gemeinde)
  pop_per_commune_1km <- emigrate_distance[1]/length(communes_name_1k)

  
  
  distance_1km <- st_is_within_distance(st_centroid(commune_start),ha_geo,dist=1000)[[1]]
  ha_1km <- ha_geo[distance_1km,]
  #print(baden_hectare_map(ha_1km$geometry,ha_1km$B21BTOT,"pop"))
  
  print("5km")
  distance_5km <- st_is_within_distance(st_centroid(commune_start),ha_geo,dist=5000)[[1]]
  ha_5km <- ha_geo[distance_5km,]
  distance_5km <- st_difference(ha_5km$geometry,commune_start$geometry)
  ha_5km <- ha_geo[distance_5km,]
  #print(baden_hectare_map(ha_5km$geometry,ha_5km$B21BTOT,"pop"))
  
  communes_5km <- unique(ha_5km$Gemeinde)
  
  ha_5km_polygon <- st_boundary(ha_5km)
  
  
  print("10km")
  distance_10km <- st_is_within_distance(st_centroid(commune_start),ha_geo,dist=10000)[[1]]
  #ha_10km <- ha_geo[distance_10km,]
  #distance_10km <- st_difference(ha_10km$geometry,ha_5km_polygon$geometry)
  #ha_10km <- ha_geo[distance_10km,]
  #print(baden_hectare_map(ha_10km$geometry,ha_10km$B21BTOT,"pop"))
  
  
}

emigration <- function(pop) {
  #'pop is pop_v
  #'limit refers to min amount of people you want to indicate an arrow with
  arrow_data <- data.frame(x1 = 0,
                           x2 = 0,
                           y1 = 0,
                           y2 = 0,
                           people = 0)
  for (commune in 1:length(communes)) {
    net_m <- as.integer(pop$net_migration)[commune]
    #print(net_m)
    c_geo <- filter(commune_geo, Gemeinde == communes[commune])
    neighbour_communes_distance <- st_is_within_distance(c_geo,commune_geo,dist=2000)[[1]]
    #print(neighbour_communes_distance)
    neighbour_communes_geo <- commune_geo[neighbour_communes_distance,]%>%
      setdiff(c_geo)
    neighbour_communes <- (neighbour_communes_geo$Gemeinde)
    #print(neighbour_communes)
    tot_pop_neighbour_commune <- sum(neighbour_communes_geo$population)
    
    #print(neighbour_communes)
    #print(neighbour_communes_geo)
    for (neighbour in 1:length(neighbour_communes)) {
      n_geo <- st_centroid(neighbour_communes_geo[neighbour,])
      migration <- n_geo$population/tot_pop_neighbour_commune*net_m
      #People will travel to larger communes rather than small ones
      
      if (net_m < 0) { #people move out
        start_point <- st_coordinates(st_centroid(c_geo))
        end_point <- st_coordinates(n_geo)
      }
      else { #people move in
        start_point <- st_coordinates(n_geo)
        end_point <- st_coordinates(st_centroid(c_geo))
      }
      start_x <- start_point[1]
      start_y <- start_point[2]
      end_x <- end_point[1]
      end_y <- end_point[2]
      arrow_data <- rbind(arrow_data,c(start_x,end_x,start_y,end_y,migration))
    }
  }
  arrow_data <- arrow_data[-1,]
  return(arrow_data)
}

arrow_plot <- function (plot, arrow_data, limit) {
  #arrow data needs to be this format: x1,x2,y1,y2,people
  #limit refers to amount of people is required before an arrow is drawn
  arrow_data <- filter(arrow_data,abs(people) > limit)
  p <- plot + new_scale_colour() +
    geom_segment(data=arrow_data, 
                 aes(x=x1,y=y1,xend=x2,yend=y2,color=people),
                 arrow = arrow(angle=40,length=unit(0.3,"cm"),ends="last",type="closed"),
                 alpha = 0.8,
                 linewidth = 1.2,
                 position = position_attractsegment(start_shave = 0.1, 
                                                    end_shave = 0.1))+
    scale_color_viridis(
      name = paste("People Migrating, at least", limit, "people")
    )
  return(p)
}
coords <- (emigration(pop_v))

final_plot <- arrow_plot(p, coords, 30)

final_plot


#Distance Calculation ----
for (commune in 1:length(communes)) {
  emigration_distance <- sample(2:6,size=1,prob=move_out[commune,2:5])#Gives a random distance the people will move out
  ha_in_commune <- filter(ha_geo,Gemeinde == communes[commune])
  ha <- runif(1, min=1, max=nrow(ha_in_commune))
  baden_hectare_map(ha,ha$B21BTOT,"hectare")
}

lower_lim <- c(-1,1001,5001,10001,50001)
upper_lim <- c(1000,5000,1000,50000,50001)




baden_hectare_map(ha_geo,ha_geo$B21BTOT,"hectare")
                                                                                                  
#near_Baden_10k <- st_is_within_distance(commune_geo$geometry[1],commune_geo,dist=5000)[[1]]
#near_Baden_50k <- st_is_within_distance(commune_geo$geometry[1],commune_geo,dist=10000)[[1]]%>%
#  setdiff(near_Baden_10k)

#baden_commune_map(commune_geo[near_Baden_50k,],commune_geo$population[near_Baden_50k],"population")


#Simulation ----
factors <- colnames(factor_pop)[-c(1:27)]
factor_change <- data.frame(factors = factors, change1 = 1, change2 = 1, change3 = 1, change4 = 1, change5 = 1)
factor_change$factors <- gsub("\\(AG)|", "",factor_change$factors)
factor_change$factors <- gsub("\\..AG.|", "",factor_change$factors)

factor_change <- as.data.frame(t(factor_change))
colnames(factor_change) <- factor_change[1,]
factor_change <- dplyr::select(factor_change, starts_with(c("Year","house","rent","ent_","edu","health")))




simulation <- function(slider_input) {
  #' slider_input is a df containing all interactions with user
  #' data is the 
  #Extract data 
  f_change <- factor_change
  df <- as.data.frame(t(slider_input))
  colnames(df) <- df[1,]
  commune <- dplyr::select(df,contains("Commune"))[2,]
  health <- dplyr::select(df,contains("health"))[2,]
  house <- dplyr::select(df,contains("house"))[2,]
  rent <- dplyr::select(df,contains("rent"))[2,]
  ent <- dplyr::select(df,contains("Job"))[2,]
  edu <- dplyr::select(df,contains("Education"))[2,]
  cat(commune,health,house,rent,ent,edu)
  
  f_change <- dplyr::select(f_change,ends_with(paste0("_",commune)))
  
  factor_name <- c("house","rent","ent","edu","health")
  factor <- c(house,rent,ent,edu,health)
  f_change[1,] <- commune 
  for (i in 1:5) {
    f <- paste0(factor_name[i],"_",commune)
    #print(f)
    #print(f_change[[f]])
    
    factor_i <- as.numeric(factor[i])
    #print(factor[i])
    #print(factor_i)
    f_change[[f]][2] <- factor_i
  }
  return(f_change)
}

x <- simulation(slider_demo)
view(x)

factor_to_pop <- function(factor_change_df,factor_value_df,pop) {
  #'chose THE COLUMN IN YOUR POP DF YOU WANT CHANGED
  print("start factor to pop")
  commune <- factor_change_df[1,1]
  
  c <- which(communes==commune)
  for (i in 1:length(communes)) {
    coef_commune <- paste0("coef_",communes[i])
    influential_factors <- get(coef_commune)$coefficient #includes intercept
    #print(c(influential_factors,"factors"))
    influential_values <- get(coef_commune)$value
    #print(c(influential_values,"value"))
    #cat(influential_factors,influential_values,"influential factors, values")
    pop[i] <- influential_values[1] #set population to intercept
    for (f in 2:length(influential_factors)) {
      coef <- influential_factors[f]
      #print(c(coef,"coef"))
      factor_change <- 1
      if (c==i) {
        factor_change <- as.numeric(dplyr::select(factor_change_df,starts_with(coef))[2,])
      }
      factor_value <- as.numeric(dplyr::select(factor_value_df,starts_with(coef))[1,])
      #print(c(factor_value,"value"))
      #print(c(factor_change,"change"))
      #print(c(pop[c],influential_values[i],factor_change*factor_value))
      pop[i] <- pop[i]+influential_values[f]*factor_change*factor_value
    }
    
    pop[i]<- pop[i]*runif(1,min=0.95,max=1.05)
  }
    
  
  return(pop)
}

pop_v$new_pop <- factor_to_pop(x,fpop,pop_v$new_pop)

pop_v <- pop_analysis(pop_v)

plot_df <- commune_geo

plot_df$new_pop <- pop_v$new_pop


p<-baden_commune_map(plot_df,plot_df$new_pop,"population")
p


