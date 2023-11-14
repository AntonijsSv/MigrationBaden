#Program to plot and analyze data
# Libraries ----
library(tidyverse)
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(patchwork)
library(ggnewscale)
library(corrplot)
library(car)

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
statent <- read_xlsx("analysis/STATENT_Communes.xlsx")
statent_geo <- left_join(commune_geo,statent,by=join_by(Gemeinde))

house_prices <- read_excel("analysis/Houseprices.xlsx")
house_prices <- house_prices[-c(1,2),-(15:ncol(house_prices))]
colnames(house_prices) <- c("Gemeinden",2023:2011)

rent_prices <- read_csv("rent_prices.csv")
rentprices <- rent_prices[-c(1:4),c(2,23:ncol(rent_prices))]
colnames(rentprices) <- c("Gemeinden",2021:2011)
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

# Data Manipulation Functions ----
dataset <- function(data,value) {
  rownames(data) <- data[,1]
  data <-  dplyr::select(data,contains(value)) #only select values to plot
  colnames(data) <- gsub("(.*?)_(\\d+)", "\\2", colnames(data)) #Finds the year in the colnames and extracts the year
  data <- t(data) 
  data <- cbind(rownames(data),data.frame(data, row.names = NULL))
  colnames(data)[1] <- "Years"
  return(data)
  
}
multiple_values <- function(data,values) {
  df <- (dataset(as.data.frame(data),values[1]))
  for (i in 2:length(values)) {
    single_value <- (dataset(as.data.frame(data),values[i]))
    df[,2:ncol(df)] <- df[,2:ncol(df)] + single_value[,2:ncol(single_value)]
    #We start at 2 as the first column is the year and we dont need to add those values
  }
  return(df)
}


# job opportunities ----
statent_tot <- (dataset(as.data.frame(statent),"B08T"))
colnames(statent_tot)[2:ncol(statent_tot)] <- paste0("ent_",colnames(statent_tot)[2:ncol(statent_tot)])

factor_pop <- full_join(pop,statent_tot, by = "Years")


#House & Rent Prices ---
house <- dataset(as.data.frame(house_prices),"2")
colnames(house)[2:ncol(house)] <- paste0("house_",colnames(house)[2:ncol(house)])
for (col in 2:ncol(house)) {
  house[[col]] <- as.double(house[[col]])
}
factor_pop <- full_join(factor_pop,house, by = "Years")

rent <- dataset(as.data.frame(rentprices),"2")
colnames(rent)[2:ncol(rent)] <- paste0("rent_",colnames(rent)[2:ncol(rent)])

factor_pop <- full_join(factor_pop,rent, by = "Years")

#Education ----
edu <- dataset(as.data.frame(statent),"B0885AS")
colnames(edu)[2:ncol(edu)] <- paste0("edu_",colnames(edu)[2:ncol(edu)])

factor_pop <- full_join(factor_pop,edu, by = "Years")

#Health ----
health <- dataset(as.data.frame(statent),"B0886AS")
colnames(health)[2:ncol(health)] <- paste0("health_",colnames(health)[2:ncol(health)])
factor_pop <- full_join(factor_pop,health, by = "Years")

for (col in 1:ncol(factor_pop)) {
  colnames(factor_pop[col]) <- gsub("\\..AG).", "", colnames(factor_pop[col]))%>%
    str_trim()
  factor_pop[[col]] <- as.numeric(factor_pop[[col]])
}

# Factor_Pop Final Manipulation ----
last_yr <- factor_pop[13,]
factor_pop <- factor_pop[-13,]
factor_pop <- rbind(last_yr,factor_pop)

factor_pop$house_Neuenhof <- factor_pop$house_Neuenhof[c(1,7:13,2:6)] #The data somehow got switched up in procedures before

write.csv(factor_pop,"final_dataset.csv")
fp <- filter(factor_pop,Years == 2021)
write.csv(fp,"final_dataset_21.csv")
 
# fpop creation ----
df0 <-
  factor_pop |>
  dplyr::select(-contains("pop_")) |>
  pivot_longer(-Years) |>
  mutate(underscore = str_locate(name, "_")[, 1]) |>
  transmute(
    Years,
    factor = str_sub(name, 1, underscore - 1),
    commune = str_sub(name, underscore + 1, -1),
    value
  )

df1 <-
  factor_pop |>
  dplyr::select(Years, contains("pop_")) |>
  pivot_longer(-Years) |>
  mutate(underscore = str_locate(name, "_")[, 1]) |>
  transmute(Years, commune = str_sub(name, underscore + 1, -1), pop = value)

df0$commune <- gsub("\\(AG)|", "",df0$commune)%>%
  str_trim()
df0$commune <- gsub("\\..AG.|", "",df0$commune)%>%
  str_trim()
df1$commune <- gsub("\\(AG)|", "",df1$commune)%>%
  str_trim()
df1$commune <- gsub("\\..AG.|", "",df1$commune)%>%
  str_trim()


fpop <- full_join(df0, df1, c("Years", "commune"))



# fpop plotting ----
fpop_factor_plot <- function(f,title) {
  #' f is the character name of the factor
  windows()
  fpop |>
    filter(factor == f) |>
    ggplot() +
    geom_path(aes(value, pop)) +
    geom_point(aes(value, pop, colour = Years), size = 2) +
    scale_colour_viridis_b() +
    facet_wrap(~commune, 7, 4, "free") +
    theme_bw(base_size = 12)+
    labs(
      title = title
    )
}

fpop_factor_plot("edu","Education")




# Corrplots ----

factors <- c("ent","house","rent","edu","health","entertainment")
correlations <- paste0("cor_",factors)
for (factor in 1:length(factors)) {
  cor_factor_name <- correlations[factor]
  x <- dplyr::select(factor_pop, starts_with(factors[factor]))
  y <- dplyr::select(factor_pop,starts_with("pop"))
  y <- y[complete.cases(x),]
  x <- x[complete.cases(x),]
  x <- x[complete.cases(y),]
  y <- y[complete.cases(y),]
  
  cor_factor <- cor(x,y)
  assign(correlations[factor],cor_factor)
}


# Commune Plots ----
commune_plot <- function(commune,lag){
  windows()
  par(mfrow=c(2,5),mar=c(3,3,3,1))
  c <- gsub("\\(AG)|", "",commune) %>%
    str_trim()
  commune_pop <- paste0("pop_",c)
  for (i in 1:length(factors)) {
    #factor on the x-axis
    commune_factor <- colnames(dplyr::select(factor_pop, contains(c) & starts_with(paste0(factors[i],"_"))))[1]
    
    y <- factor_pop[[commune_pop]][-is.na(factor_pop[[commune_factor]])]
    x <- factor_pop[[commune_factor]][-is.na(factor_pop[[commune_factor]])]
    x <- factor_pop[[commune_factor]][-is.na(factor_pop[[commune_pop]])]
    y <- factor_pop[[commune_pop]][-is.na(factor_pop[[commune_pop]])]
    
    if(missing(lag)) {
      plot(x,y,main=c,xlab=factors[i],ylab="",line=1,pch = 16, col = "#005588")
    }
    else {
      plot(x[-c(1:lag)],y[-c(length(y):length(y)-lag)],main=c,xlab=factors[i],ylab="",line=1,pch = 16, col = "#005588")
    }
    
    try(abline(lm(as.formula(paste0(commune_pop,"~",commune_factor)), data = factor_pop)))
    
  }
}

# Factor Plots ----
factor_plot <- function(factor,df,lag){
  
  for (i in 1:length(commune)) {
    c <- gsub("\\(AG)|", "",commune[i]) %>%
      str_trim()
    #population on the y-axis
    commune_pop <- paste0("pop_",c)
    #factor on the x-axis
    commune_factor <- colnames(dplyr::select(df, contains(c) & starts_with(paste0(factor,"_"))))[1]
    
    y <- df[[commune_pop]][-is.na(df[[commune_factor]])]
    x <- df[[commune_factor]][-is.na(df[[commune_factor]])]
    x <- df[[commune_factor]][-is.na(df[[commune_pop]])]
    y <- df[[commune_pop]][-is.na(df[[commune_pop]])]
    
    if(missing(lag)) {
      plot(x,y,main=c,xlab=factor,ylab="",line=1,pch = 16, col = "#005588")
    }
    else {
      plot(x[-c(1:lag)],y[-c(length(y):(length(y)-lag+1))],main=paste(c,"lag:",lag),xlab=factor,ylab="",line=1,pch = 16, col = "#005588")
    }
  
    try(abline(lm(as.formula(paste0(commune_pop,"~",commune_factor)), data = df)))
    
  }
}



#Plot all for 1 factor
#"gws","ent","services","house","rent","edu","health","entertainment"

factor_plot("house",factor_pop)

acf(factor_pop$rent_Baden,na.action = na.pass)

# Cor Plots ----
corr_matrix_factor <- function(factor_x,factor_y,df,coef_col){
  x <- dplyr::select(df, starts_with(paste0(factor_x,"_")))
  y <- dplyr::select(df,starts_with(paste0(factor_y,"_")))

  y <- y[complete.cases(x),]
  x <- x[complete.cases(x),]
  x <- x[complete.cases(y),]
  y <- y[complete.cases(y),]
  cor_factor <- cor(x,y)

  corrplot(cor_factor,tl.cex = 0.8,number.cex = 0.8,title=paste(factor_y,"vs",factor_x),mar=c(0,0,1,0),addCoef.col = coef_col)
}

corr_matrix_commune <- function(commune,df){
  x <- dplyr::select(df, contains(paste0("_",commune)))
  y <- dplyr::select(df,starts_with("pop_"))

  y <- y[complete.cases(x),]
  x <- x[complete.cases(x),]
  x <- x[complete.cases(y),]
  y <- y[complete.cases(y),]
  cor_factor <- cor(x,y)
  windows()
  par(mar=c(1,1,1,1),cex.axis=0.7, cex.lab= 0.7)
  corrplot(cor_factor,tl.cex = 0.8,number.cex = 0.8,title=commune,mar=c(0,0,1,0),addCoef.col = "gray50")
}

cm_commune_factor <- function(commune,df){
  x <- dplyr::select(df, contains(paste0("_",commune)))
  x <- x[complete.cases(x),]
  cor_factor <- cor(x,x)
  windows()
  par(mar=c(1,1,1,1),cex.axis=0.7, cex.lab= 0.7)
  corrplot(cor_factor,tl.cex = 0.8,number.cex = 0.8,title=commune,mar=c(0,0,1,0),addCoef.col = "gray50")
}

windows()
par(mar=c(1,1,1,1),cex.axis=0.7, cex.lab= 0.7)
corrplot(cor_house,tl.cex = 0.8,number.cex = 0.8,title="house",mar=c(0,0,1,0),addCoef.col = "black")

corr_matrix_factor("house","pop",factor_pop,"transparent")
corr_matrix_commune("Baden",factor_pop)
# Pair Plots ----
pair_plot <- function(commune){
  windows()
  par(mar=c(1,1,1,1))
  df <- dplyr::select(factor_pop, contains(paste0("_",commune)))
  pairs(df)
}
pair_plot("Gebenstorf")


# Factors ----
factors <- c("ent","house","rent","edu","health","entertainment")
factors_v2 <- c("ent", "house", "edu")

yrs <- factor_pop$Years
# Analysis ----
commune # list of all communes
factors # List of all factors
commune_plot("Bellikon")

windows()
par(mfrow=c(5,6),mar=c(3,3,3,1))
factor_plot("health",factor_pop)


corr_matrix_commune("Baden",factor_pop)

windows()
par(mar=c(1,1,1,1),cex.axis=0.7, cex.lab= 0.7)
corr_matrix_factor("house","rent",factor_pop,"transparent")

cm_commune_factor ("Neuenhof",factor_pop)

fpop_factor_plot("ent")

pair_plot("Birmenstorf")

windows()
par(mfrow=c(5,6),mar=c(3,3,3,1))
plot(factor_pop$Years,factor_pop$pop_Birmenstorf)

x <- dplyr::select(factor_pop,contains("_Birmenstorf"))
x <- dplyr::select(x, starts_with(c("ent_", "house_", "rent_", "edu_", "health")))
windows()
par(mar=c(1,1,1,1))
pairs(x,cex.labels= 1.5,cex.axis=1.5)

# Multi-Variable Lm regression ----
lm_Baden <- lm(pop_Baden~rent_Baden
               + edu_Baden
               + ent_Baden
               + health_Baden
               ,data = factor_pop)
summary(lm_Baden)
plot(lm_Baden)
AIC(lm_Baden)
vif(lm_Baden)

lm_Bellikon <- lm(pop_Bellikon~rent_Bellikon
                  + edu_Bellikon
                  + ent_Bellikon
                  + health_Bellikon
                    ,data = factor_pop)
summary(lm_Bellikon)
plot(lm_Bellikon)
vif(lm_Bellikon)
AIC(lm_Bellikon)

lm_Bergdietikon <- lm(pop_Bergdietikon~rent_Bergdietikon
                        +edu_Bergdietikon
                        +health_Bergdietikon
                        +ent_Bergdietikon
                        +ent_Bellikon
                        , data = factor_pop)
summary(lm_Bergdietikon)
plot(lm_Bergdietikon)
vif(lm_Bergdietikon)

lm_Birmenstorf <- lm(pop_Birmenstorf~house_Birmenstorf..AG.
                       +ent_Birmenstorf
                       +health_Birmenstorf 
                       +edu_Birmenstorf
                     , data = factor_pop)
summary(lm_Birmenstorf)
plot(lm_Birmenstorf)
vif(lm_Birmenstorf)

lm_Ennetbaden <- lm(pop_Ennetbaden~rent_Ennetbaden
                    + health_Ennetbaden
                    + ent_Ennetbaden 
                    + edu_Ennetbaden
                      , data = factor_pop)
summary(lm_Ennetbaden)
plot(lm_Ennetbaden)
vif(lm_Ennetbaden)
AIC(lm_Ennetbaden)

lm_Fislisbach <- lm(pop_Fislisbach~house_Fislisbach
                    +  health_Fislisbach
                    +  edu_Fislisbach
                    + ent_Fislisbach
                    , data = factor_pop)
summary(lm_Fislisbach)
plot(lm_Fislisbach)
vif(lm_Fislisbach)

lm_Freienwil <- lm(pop_Freienwil~house_Freienwil
                   #+ health_Freienwil
                   + ent_Freienwil
                   + edu_Freienwil
                   , data = factor_pop)
summary(lm_Freienwil)
vif(lm_Freienwil)
plot(lm_Freienwil)

lm_Gebenstorf <- lm(pop_Gebenstorf~house_Gebenstorf
                    +ent_Gebenstorf
                    +health_Gebenstorf
                    +edu_Gebenstorf
                    
                    , data =factor_pop)
summary(lm_Gebenstorf)
plot(lm_Gebenstorf)
vif(lm_Gebenstorf)

lm_Killwangen <- lm(pop_Killwangen~house_Killwangen+ent_Killwangen
                    + edu_Killwangen
                    + health_Killwangen
                    , data = factor_pop)
summary(lm_Killwangen)
plot(lm_Killwangen)
vif(lm_Killwangen)

lm_Künten <- lm(pop_Künten~house_Künten
                + health_Künten
                + edu_Künten
                + ent_Künten
                , data = factor_pop)
summary(lm_Künten)
plot(lm_Künten)
vif(lm_Künten)
AIC(lm_Künten)

lm_Mägenwil <- lm(pop_Mägenwil~rent_Mägenwil+ent_Mägenwil
                  + edu_Mellingen
                  + edu_Mägenwil
                  + health_Mägenwil
                    , data = factor_pop)
summary(lm_Mägenwil)
plot(lm_Mägenwil)
vif(lm_Mägenwil)

lm_Mellingen <- lm(pop_Mellingen~rent_Mellingen 
                   + health_Mellingen
                   + edu_Mellingen
                   + ent_Mellingen
                   + ent_Fislisbach
                   
                   , data = factor_pop)
summary(lm_Mellingen)
plot(lm_Mellingen)
vif(lm_Mellingen)

lm_Neuenhof <- lm(pop_Neuenhof~house_Neuenhof 
                  + health_Neuenhof 
                  + edu_Neuenhof
                  + ent_Neuenhof
                    , data = factor_pop)
summary(lm_Neuenhof)
vif(lm_Neuenhof)
AIC(lm_Neuenhof)
plot(lm_Neuenhof)

lm_Niederrohrdorf <- lm(pop_Niederrohrdorf~house_Niederrohrdorf
                        + ent_Niederrohrdorf
                        + edu_Niederrohrdorf
                        + health_Niederrohrdorf
                          , data = factor_pop)
summary(lm_Niederrohrdorf)
vif(lm_Niederrohrdorf)
plot(lm_Niederrohrdorf)

lm_Oberrohrdorf <- lm(pop_Oberrohrdorf~house_Oberrohrdorf
                      + ent_Oberrohrdorf
                      + edu_Oberrohrdorf
                      + health_Oberrohrdorf
                      , data = factor_pop)
summary(lm_Oberrohrdorf)
vif(lm_Oberrohrdorf)
plot(lm_Oberrohrdorf)

lm_Obersiggenthal <- lm(pop_Obersiggenthal~house_Obersiggenthal
                        + health_Obersiggenthal
                        + ent_Obersiggenthal
                        + edu_Obersiggenthal
                        , data = factor_pop)
summary(lm_Obersiggenthal) 
vif(lm_Obersiggenthal)
plot(lm_Obersiggenthal)

lm_Remetschwil <- lm(pop_Remetschwil~rent_Remetschwil
                     + edu_Remetschwil
                     + health_Remetschwil
                     + ent_Remetschwil
                     , data = factor_pop)
summary(lm_Remetschwil)
plot(lm_Remetschwil)

lm_Spreitenbach <- lm(pop_Spreitenbach~rent_Spreitenbach 
                      + health_Spreitenbach
                      + ent_Spreitenbach
                      + edu_Spreitenbach
                      , data = factor_pop)
summary(lm_Spreitenbach)
vif(lm_Spreitenbach)
plot(lm_Spreitenbach)

lm_Stetten <- lm(pop_Stetten~rent_Stetten
                 +health_Stetten
                 +edu_Stetten
                 +ent_Stetten
                 , data =factor_pop)
summary(lm_Stetten)
vif(lm_Stetten)
plot(lm_Stetten)

lm_Turgi <- lm(pop_Turgi~house_Turgi 
               + ent_Turgi
               + health_Turgi
               + edu_Turgi
                 , data = factor_pop)
summary(lm_Turgi)
plot(lm_Turgi)
AIC(lm_Turgi)
vif(lm_Turgi)

lm_Untersiggenthal <- lm(pop_Untersiggenthal~house_Untersiggenthal
                         + ent_Untersiggenthal
                         + health_Untersiggenthal
                         + edu_Untersiggenthal
                         , data = factor_pop)
summary(lm_Untersiggenthal)
vif(lm_Untersiggenthal)
plot(lm_Untersiggenthal)

lm_Wettingen <- lm(pop_Wettingen~rent_Wettingen
                   +edu_Wettingen
                   +health_Wettingen
                   +ent_Wettingen
                   , data = factor_pop)
summary(lm_Wettingen)
vif(lm_Wettingen)
lm_diagnosis(lm_Wettingen)

lm_Wohlenschwil <- lm(pop_Wohlenschwil~house_Wohlenschwil
                      +health_Wohlenschwil
                      +edu_Wohlenschwil
                      +ent_Wohlenschwil
                      , data = factor_pop)
summary(lm_Wohlenschwil)
plot(lm_Wohlenschwil)

lm_Würenlingen <- lm(pop_Würenlingen~rent_Würenlingen
                     + health_Würenlingen
                     + edu_Würenlingen
                     + ent_Würenlingen
                     + ent_Untersiggenthal
                       , data =factor_pop)
summary(lm_Würenlingen)
vif(lm_Würenlingen)
plot(lm_Würenlingen)

lm_Würenlos <- lm(pop_Würenlos~house_Würenlos
                  + health_Würenlos
                  + edu_Würenlos
                  + ent_Würenlos
                  + health_Neuenhof
                  , data =factor_pop)
summary(lm_Würenlos)
vif(lm_Würenlos)
plot(lm_Würenlos)

lm_Ehrendingen <- lm(pop_Ehrendingen~rent_Ehrendingen
                     + health_Ehrendingen
                     + edu_Ehrendingen
                     + ent_Ehrendingen
                     + health_Ennetbaden
                     , data =factor_pop)
summary(lm_Ehrendingen)
vif(lm_Ehrendingen)
plot(lm_Ehrendingen)
# Population Growth ----
pop_growth <- dplyr::select(factor_pop, contains("Year") |contains("pop"))
pop_growth$pop_tot <- 0
pop_growth$pop_growth <- 0

for (yr in 1:nrow(pop_growth)) {
  pop_growth$pop_tot[yr] <- sum(pop_growth[yr,c(2:27)], na.rm=TRUE)
}
for (yr in 1:nrow(pop_growth)) {
  pop_growth$pop_growth[yr] <- pop_growth$pop_tot[yr]-pop_growth$pop_tot[yr+1]
}
pop_growth <- dplyr::select(pop_growth,Years,pop_growth,pop_tot)
pop_growth <- pop_growth[-c(1,13),]
pop_growth$percent <- pop_growth$pop_growth/pop_growth$pop_tot
mean_growth <- mean(pop_growth$percent)

# VIF ----
for (c in 1:length(communes)) {
  coef <- get(paste0("lm_",communes[c]))
  print(vif(coef))
}
