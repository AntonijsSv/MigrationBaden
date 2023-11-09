#Program to plot data
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
gws <- read_csv("analysis/GWS_Communes.csv") %>%
  dplyr::select(-"...1")
gws_geo <- left_join(commune_geo,gws,by=join_by(Gemeinde==Communes))
statent <- read_xlsx("analysis/STATENT_Communes.xlsx")
statent_geo <- left_join(commune_geo,statent,by=join_by(Gemeinde))

ov <- read_csv("OeV/OV_Baden_communes_2023_2013.csv")

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

# ggplot ----
double_analysis <- function(df,
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
  
  data_plot <- ggplot(data=df)+
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
# Factor_pop creation ----
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
# gws ---
#gws_tot <- (dataset(as.data.frame(gws),"GTOT"))
#colnames(gws_tot)[2:ncol(gws_tot)] <- paste0("gws_",colnames(gws_tot)[2:ncol(gws_tot)])

# job opportunities ---
statent_tot <- (dataset(as.data.frame(statent),"B08T"))
colnames(statent_tot)[2:ncol(statent_tot)] <- paste0("ent_",colnames(statent_tot)[2:ncol(statent_tot)])
statent_pop <-left_join(statent_tot,pop, by = "Years") 

#gws_ent <- full_join(gws_tot,statent_tot, by = "Years")


factor_pop <- full_join(pop,statent_tot, by = "Years")


# services ---
services <- c("B0835AS",
              "B0836AS",
              "B0837AS",
              "B0853AS",
              "B0856AS",
              "B0859AS",
              "B0865AS",
              "B0886AS",
              "B0888AS",
              "B0891AS",
              "B0893AS")
#List of relevant services in statent
services_tot <- multiple_values(statent,services)
colnames(services_tot)[2:ncol(services_tot)] <- paste0("services_",commune)

factor_pop <- full_join(factor_pop,services_tot, by = "Years")

#Public Transport ---
pt <- c("train","bus")
public_transport <- multiple_values(ov,pt)
colnames(public_transport)[2:ncol(public_transport)] <- paste0("ov_",commune)
for (col in 1:ncol(public_transport)) {
  public_transport[[col]] <- as.double(public_transport[[col]])
}
#factor_pop <- full_join(factor_pop,public_transport, by = "Years")

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

#Education
edu <- dataset(as.data.frame(statent),"B0885AS")
colnames(edu)[2:ncol(edu)] <- paste0("edu_",colnames(edu)[2:ncol(edu)])

factor_pop <- full_join(factor_pop,edu, by = "Years")

#Handel
shop <- multiple_values(statent,c("B0846AS","B0847AS"))
colnames(shop)[2:ncol(shop)] <- paste0("shop_",colnames(shop)[2:ncol(shop)])
#factor_pop <- full_join(factor_pop,shop, by = "Years")
#Health
health <- dataset(as.data.frame(statent),"B0886AS")
colnames(health)[2:ncol(health)] <- paste0("health_",colnames(health)[2:ncol(health)])
factor_pop <- full_join(factor_pop,health, by = "Years")
#Entertainment
entertainment <- multiple_values(statent,c("B0890AS","B0893AS"))
colnames(entertainment)[2:ncol(entertainment)] <- paste0("entertainment_",colnames(entertainment)[2:ncol(entertainment)])
factor_pop <- full_join(factor_pop,entertainment, by = "Years")

for (col in 1:ncol(factor_pop)) {
  colnames(factor_pop[col]) <- gsub("\\(AG)|", "", colnames(factor_pop[col]))%>%
    str_trim()
  factor_pop[[col]] <- as.numeric(factor_pop[[col]])
}

gastro <- dataset(as.data.frame(statent),"B0858AS")
colnames(gastro)[2:ncol(health)] <- paste0("gastro_",colnames(gastro)[2:ncol(gastro)])
for (col in 1:ncol(gastro)) {
  gastro[[col]] <- as.numeric(gastro[[col]])
}
#factor_pop <- full_join(factor_pop,gastro, by = "Years")

last_yr <- factor_pop[13,]
factor_pop <- factor_pop[-13,]
factor_pop <- rbind(last_yr,factor_pop)

factor_pop$house_Neuenhof <- factor_pop$house_Neuenhof[c(1,7:13,2:6)] #The data somehow got switched up in procedures before

# d_factor_pop ----
d_factor_pop <- factor_pop
for (col in 1:ncol(d_factor_pop)) {
  d_factor_pop[[col]] <- as.numeric(d_factor_pop[[col]])
}

for (row in 1:nrow(d_factor_pop)) {
  rownames(d_factor_pop) [row] <- paste0(d_factor_pop[row+1,1],"_",d_factor_pop[row,1])
  d_factor_pop[row,c(2:ncol(d_factor_pop))] <- d_factor_pop[row,c(2:ncol(d_factor_pop))] - d_factor_pop[row+1,c(2:ncol(d_factor_pop))]
}
d_factor_pop <- d_factor_pop[-nrow(d_factor_pop),]

  
# fpop ----
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
fpop_factor_plot <- function(f) {
  #' f is the character name of the factor
  windows()
  fpop |>
    filter(factor == f) |>
    ggplot() +
    geom_path(aes(value, pop)) +
    geom_text(aes(value, pop, label = Years), size = 1.5) +
    facet_wrap(~commune, 5, 6, "free") +
    theme_gray(base_size = 8)
}
fpop_commune_plot <- function(c) {
  #' f is the character name of the factor
  windows()
  fpop |>
    filter(commune == c) |>
    ggplot() +
    geom_path(aes(value, pop)) +
    geom_text(aes(value, pop, label = Years), size = 1.5) +
    facet_wrap(~commune, 5, 6, "free") +
    theme_gray(base_size = 8)
}

fpop_factor_plot("house")

# Data Analysis - Regression ----

lm_ent <- lm(pop_Baden~ent_Baden, data=factor_pop)
summary(lm_ent)
lm_ov <- lm(pop_Baden~ov_Baden, data = factor_pop)
plot(lm_ov)

lm_factors<- lm(pop_Baden[-c(1,13,12)]~rent_Baden[-c(1,2,3)], data = factor_pop)
summary(lm_factors)

lm_Ehrendingen <- lm(pop_Ehrendingen~ov_Ehrendingen, data = factor_pop)
summary(lm_Ehrendingen)

lm_Birmi_house <- lm(pop_Birmenstorf~house_Birmenstorf..AG., data= factor_pop)
summary(lm_Birmi_house)

ggplot(factor_pop,aes(ov_Ehrendingen,pop_Ehrendingen))+
  geom_point(colour = "#005588")

ov_pop <- dplyr::select(factor_pop, contains("pop_")|contains("ov_"))

for (col in 1:ncol(ov_pop)){
  ov_pop[[col]] <- as.numeric(ov_pop[[col]])
}




# Corrplots ----
single_factor_pop_dfs <-paste0("pop_",factors)

#No time Delay
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

#Time Delay 1 Year

correlation_matrix <- function(factor_list, df, time_delay) {
  for (factor in 1:length(factors)) {
    cor_factor_name <- factor_list[factor]
    x <- dplyr::select(df, starts_with(paste0(factors[factor],"_")))
    y <- dplyr::select(df,starts_with("pop_"))
    y <- y[complete.cases(x),]
    x <- x[complete.cases(x),]
    x <- x[complete.cases(y),]
    y <- y[complete.cases(y),]
    #print(c(nrow(x),nrow(y)))
    if (!missing(time_delay)) {
      x <- x[-c(1:time_delay),]
      y <- y[-c(nrow(y):(nrow(y)-time_delay+1)),]
      #print(c(nrow(x),nrow(y),nrow(y)-time_delay))
    }
    #view(x)
    #view(y)
    cor_factor <- cor(x,y)
    assign(factor_list[factor],cor_factor,envir = .GlobalEnv)
  }
}
cor0 <- paste0("cor_",factors) 
cor_1 <- paste0("cor1_",factors)
cor_2 <- paste0("cor2_",factors)
cor_3 <- paste0("cor3_",factors)
cor_4 <- paste0("cor4_",factors)
dcor <- paste0("dcor_",factors)
dcor_1 <- paste0("dcor1_",factors)
dcor_2 <- paste0("dcor2_",factors)
dcor_3 <- paste0("dcor3_",factors)
dcor_4 <- paste0("dcor4_",factors)
correlation_matrix(cor0,factor_pop)


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
  windows()
  par(mfrow=c(5,6),mar=c(3,3,3,1))
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
  windows()
  par(mar=c(1,1,1,1),cex.axis=0.7, cex.lab= 0.7)
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

corr_matrix_factor("house",factor_pop)
corr_matrix_commune("Baden",factor_pop)
# Pair Plots ----
pair_plot <- function(commune){
  windows()
  par(mar=c(1,1,1,1))
  df <- dplyr::select(factor_pop, contains(paste0("_",commune)))
  pairs(df)
}
pair_plot("Gebenstorf")

# Multiple Cor Plots ----
windows()
par(mfrow=c(1,4),mar=c(3,3,3,1))


multi_cor_plot <- function(c_plot, factor_list) {
  c_pop <- colnames(dplyr::select(factor_pop,starts_with(paste0("pop_",c_plot))))[1]
  for (factor in 1:length(factor_list)){
    c_factor <- colnames(dplyr::select(factor_pop,starts_with(paste0(factor_list[factor],"_",c_plot))))[1]
    lm_formula <- as.formula(paste(c_pop,"~",c_factor))
    lm_plot <- lm(lm_formula, data=factor_pop)
    print(factor_list[factor])
    plot(lm_plot)
  }
}
baden_factors <- c("ent","health","house","rent","entertainment","edu","rent")
multi_cor_plot("Baden",baden_factors)

windows()
par(mfrow=c(3,4),mar=c(3,3,3,1))
lm_lin <- lm(pop_Baden~house_Baden, data=factor_pop)
lm_tl <- lm(pop_Baden[-1]~house_Baden[-1], data=factor_pop)
lm_squared <- lm(pop_Baden~entertainment_Baden + I(-entertainment_Baden^2), data=factor_pop)
lm_transformed <- lm(pop_Baden[-1]~house_Baden[-1] + I(exp(house_Baden[-1])), data=factor_pop)
summary(lm_tl)
summary(lm_lin)

plot(lm_lin)
plot(lm_tl)
plot(lm_squared)
plot(lm_transformed)
# lm Plots ----
windows()
par(mfrow=c(1,4),mar=c(3,3,3,1))
plot(lm_Birmi_house)

# Plots ----



windows()
par(mar=c(1,1,1,1),cex.axis=0.7, cex.lab= 0.7)

pairs(dplyr::select(factor_pop,contains("ent_"))[1:7])

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

plot(factor_pop$gws_Baden,factor_pop$pop_Baden,pch = 16, col = "#009999")
plot(factor_pop$ent_Baden,factor_pop$pop_Baden,pch = 16, col = "#009999")
plot(factor_pop$services_Baden,factor_pop$pop_Baden,pch = 16, col = "#009999")
plot(factor_pop$ov_Baden,factor_pop$pop_Baden,pch = 16, col = "#009999")
plot(factor_pop$house_Baden,factor_pop$pop_Baden,pch = 16, col = "#009999")
plot(factor_pop$house_Baden[-1],factor_pop$pop_Baden[-13],pch = 16, col = "#009999")
plot(factor_pop$house_Baden[-c(1:2)],factor_pop$pop_Baden[-c(13:12)],pch = 16, col = "#009999")
plot(factor_pop$house_Baden[-c(1:3)],factor_pop$pop_Baden[-c(13:11)],pch = 16, col = "#009999")
plot(factor_pop$rent_Baden,factor_pop$pop_Baden,pch = 16, col = "#009999")


plot(lm_ent$residuals,pch = 16, col = "#009999")
plot(cooks.distance(lm_ent),pch = 16, col = "#009999")

ggplot(data = factor_pop, aes(house_Baden,pop_Birmenstorf)) +
  geom_point(colour = "#005599",size=2)+
  xlab("House Prices per m^2 in Baden")+
  ylab("Population in Birmenstorf")+
  theme_minimal()
#

write.csv(factor_pop,"final_dataset.csv")

# Legacy ----
r_squared <- matrix(0, nrow = length(commune), ncol= 10)
rownames (r_squared) <- commune
colnames(r_squared) <- c("gws",
                         "ent",
                         "services",
                         "ov",
                         "house",
                         "rent",
                         "edu",
                         "shop",
                         "health",
                         "entertainment")
p_values <- matrix(0, nrow = length(commune), ncol= 10)
rownames (p_values) <- commune
colnames(p_values) <- c("gws",
                        "ent",
                        "services",
                        "ov",
                        "house",
                        "rent",
                        "edu",
                        "shop",
                        "health",
                        "entertainment")



for (c in 1:length(commune)) {
  com <- gsub("\\(AG)|", "",commune[c]) %>%
    str_trim()
  f_pop <- colnames(dplyr::select(factor_pop,
                                  contains(
                                    paste0(
                                      "pop_",com)
                                  )
  )
  )[1]
  for (f in 1:length(factors)) {
    factor_val <- colnames(dplyr::select(factor_pop,
                                         contains(
                                           paste0(
                                             factors[f],"_",com))))[1]
    print("0")
    formula <- as.formula(paste("[",f_pop,"~",factor_val,"]"))
    print("1")
    lm_model <- lm(formula, data = factor_pop)
    
    #r^2
    if (is.na(summary(lm_model)$adj.r.squared)) {
      print("wee-woo")
      r_squared[c,f] <- 0
    }
    else {r_squared[c,f] <- summary(lm_model)$adj.r.squared}
    
    #p-value
    if (is.null(summary(lm_model)$coefficients[8])|is.na(summary(lm_model)$coefficients[8])) {
      print("woo-wee")
      p_values[c,f] <- "rejected"
    }
    else{
      if(summary(lm_model)$coefficients[8] > 0.06 | summary(lm_model)$coefficients[7] > 0.06) {
        p_values[c,f] <- "rejected"
      }
      else{p_values[c,f] <- "accepted"}
    }
  }
}

# Factors ----
factors <- c("ent","house","rent","edu","health","entertainment")
factors_v2 <- c("ent", "house", "edu")

yrs <- factor_pop$Years
# Analysis ----
commune # list of all communes
factors # List of all factors
commune_plot("Baden")
factor_plot("house",factor_pop)
commune
corr_matrix_commune("Ehrendingen",factor_pop)

corr_matrix_factor("rent","house",factor_pop,"gray50")
cm_commune_factor ("Neuenhof",factor_pop)
fpop_factor_plot("ent")
pair_plot("Mellingen")



# Multi-Variable Lm regression ----
lm_diagnosis <- function(lm) {
  print(paste("vif: Varience Inflation Factor, coliniearity check",vif(lm)))
  print(paste("AIC: Akaike Information criterion, Amount of Information Lost by using Model",AIC(lm)))
  plot(lm)
  
}

lm_Baden <- lm(pop_Baden~rent_Baden
               #+ services_Baden
                 #ent_Baden +
                 #health_Baden +
                 #ent_Mägenwil 
                 
               
               ,data = factor_pop)
summary(lm_Baden)
plot(lm_Baden)
AIC(lm_Baden)
vif(lm_Baden)

lm_Bellikon <- lm(pop_Bellikon~ent_Killwangen
                  +ent_Wettingen + services_Bellikon
                    ,data = factor_pop)
summary(lm_Bellikon)
plot(lm_Bellikon)
vif(lm_Bellikon)
AIC(lm_Bellikon)
#Find other factors

lm_Bergdietikon <- lm(pop_Bergdietikon~rent_Bergdietikon
                        #edu_Bergdietikon +
                        #health_Bergdietikon +
                        +services_Bergdietikon
                        +ent_Bellikon
                        , data = factor_pop)
summary(lm_Bergdietikon)
plot(lm_Bergdietikon)
vif(lm_Bergdietikon)

lm_Birmenstorf <- lm(pop_Birmenstorf~house_Birmenstorf..AG.+
                       ent_Birmenstorf
                       #+health_Birmenstorf 
                       #+edu_Birmenstorf
                       #ent_Neuenhof
                       #ent_Obersiggenthal
                       
                     , data = factor_pop)
summary(lm_Birmenstorf)
plot(lm_Birmenstorf)
#try to explain through house price curve
# 13 -> influential point
vif(lm_Birmenstorf)

lm_Ennetbaden <- lm(pop_Ennetbaden~rent_Ennetbaden
                    + edu_Baden
                    #+ ent_Obersiggenthal
                    #+ health_Ehrendingen
                    #+ health_Ennetbaden
                    # + ent_Ennetbaden + edu_Ennetbaden
                      , data = factor_pop)
summary(lm_Ennetbaden)
plot(lm_Ennetbaden)
vif(lm_Ennetbaden)
AIC(lm_Ennetbaden)

lm_Fislisbach <- lm(pop_Fislisbach~house_Fislisbach
                    #+  health_Fislisbach
                    #+  edu_Fislisbach
                    + ent_Fislisbach
                    #+ edu_Niederrohrdorf
                    #+ health_Baden
                    
                    , data = factor_pop)
summary(lm_Fislisbach)
plot(lm_Fislisbach)
vif(lm_Fislisbach)

lm_Freienwil <- lm(pop_Freienwil~house_Freienwil
                   #+ health_Freienwil
                   #+ ent_Freienwil
                   #+ edu_Freienwil
                   #+ services_Freienwil
                   , data = factor_pop)
summary(lm_Freienwil)
vif(lm_Freienwil)
plot(lm_Freienwil)

lm_Gebenstorf <- lm(pop_Gebenstorf~house_Gebenstorf
                    #+ent_Gebenstorf
                    #+health_Gebenstorf
                    #+edu_Gebenstorf
                    #+ services_Gebenstorf
                    , data =factor_pop)
summary(lm_Gebenstorf)
plot(lm_Gebenstorf)
vif(lm_Gebenstorf)
# ent std. error is too much

lm_Killwangen <- lm(pop_Killwangen~house_Killwangen+ent_Killwangen
                    , data = factor_pop)
summary(lm_Killwangen)
plot(lm_Killwangen)
#ent Spreitenbach p slightly high
vif(lm_Killwangen)

lm_Künten <- lm(pop_Künten~house_Künten
                , data = factor_pop)
summary(lm_Künten)
plot(lm_Künten)
vif(lm_Künten)
AIC(lm_Künten)
#health std error too high


lm_Mägenwil <- lm(pop_Mägenwil~rent_Mägenwil+ent_Mägenwil
                  + edu_Mellingen
                    , data = factor_pop)
summary(lm_Mägenwil)
plot(lm_Mägenwil)
vif(lm_Mägenwil)
#2 influential points with edu
#0.55 ent p-val

lm_Mellingen <- lm(pop_Mellingen~rent_Mellingen 
                   # + health_Wohlenschwil#slightly too high pval
                   + ent_Fislisbach
                   , data = factor_pop)
summary(lm_Mellingen)
plot(lm_Mellingen)
vif(lm_Mellingen)
#ent std. error high
#rent has a sine wave like residuals and 1 influential point

lm_Neuenhof <- lm(pop_Neuenhof~house_Neuenhof 
                  #+ health_Neuenhof + edu_Neuenhof
                  + ent_Neuenhof
                  #+ services_Neuenhof
                  #+ ent_Killwangen
                  #+ health_Killwangen
                  #+ent_Ennetbaden
                  
                    , data = factor_pop)
summary(lm_Neuenhof)
vif(lm_Neuenhof)
AIC(lm_Neuenhof)
plot(lm_Neuenhof)
cm_commune_factor ("Neuenhof",factor_pop)

lm_Niederrohrdorf <- lm(pop_Niederrohrdorf~house_Niederrohrdorf
                        + edu_Oberrohrdorf
                          , data = factor_pop)
summary(lm_Niederrohrdorf)
vif(lm_Niederrohrdorf)
plot(lm_Niederrohrdorf)

lm_Oberrohrdorf <- lm(pop_Oberrohrdorf~house_Oberrohrdorf
                      #+ health_Remetschwil
                      #+ edu_Mellingen
                      #+ ent_Oberrohrdorf
                      #+ edu_Oberrohrdorf
                      , data = factor_pop)
summary(lm_Oberrohrdorf)
vif(lm_Oberrohrdorf)
plot(lm_Oberrohrdorf)

lm_Obersiggenthal <- lm(pop_Obersiggenthal~house_Obersiggenthal
                        #+ health_Obersiggenthal
                        + ent_Obersiggenthal
                        #+ edu_Ennetbaden
                        , data = factor_pop)
summary(lm_Obersiggenthal) 
vif(lm_Obersiggenthal)
plot(lm_Obersiggenthal)
#ent 0.6 pval not so good

lm_Remetschwil <- lm(pop_Remetschwil~NULL
                     , data = factor_pop)
summary(lm_Remetschwil)
plot(lm_Remetschwil)
#nothingAAAA

lm_Spreitenbach <- lm(pop_Spreitenbach~rent_Spreitenbach + health_Spreitenbach
                      #+ ent_Spreitenbach
                      #+ edu_Spreitenbach
                      #+ ent_Würenlos
                      , data = factor_pop)
summary(lm_Spreitenbach)
vif(lm_Spreitenbach)
plot(lm_Spreitenbach)
#house s curve
#ent 0.9 pval
#health ok but pairs doesnt say so


lm_Stetten <- lm(pop_Stetten~rent_Stetten
                 #+health_Stetten
                 #+edu_Stetten
                 #+ent_Stetten
                 #+edu_Mellingen
                 + edu_Baden
                 , data =factor_pop)
summary(lm_Stetten)
vif(lm_Stetten)
plot(lm_Stetten)

lm_Turgi <- lm(pop_Turgi~house_Turgi + ent_Turgi, data = factor_pop)
summary(lm_Turgi)
plot(lm_Turgi)
AIC(lm_Turgi)
vif(lm_Turgi)

lm_Untersiggenthal <- lm(pop_Untersiggenthal~house_Untersiggenthal + ent_Untersiggenthal
                         #+health_Untersiggenthal
                         #+edu_Untersiggenthal
                         #+ health_Würenlingen
                         
                         , data = factor_pop)
summary(lm_Untersiggenthal)
vif(lm_Untersiggenthal)
plot(lm_Untersiggenthal)
#ent_Untersiggenthal good aswell -> pairs
cm_commune_factor ("Untersiggenthal",factor_pop)
vif(lm_Untersiggenthal)
lm_diagnosis(lm_Untersiggenthal)

lm_Wettingen <- lm(pop_Wettingen~rent_Wettingen
                   #+edu_Wettingen
                   #+health_Wettingen
                   #+ent_Wettingen
                   #+ ent_Würenlos
                   #+ health_Würenlos
                   #+ edu_Würenlos
                   
                   , data = factor_pop)
summary(lm_Wettingen)
vif(lm_Wettingen)
lm_diagnosis(lm_Wettingen)

lm_Wohlenschwil <- lm(pop_Wohlenschwil~house_Wohlenschwil
                      #+health_Wohlenschwil
                      #+edu_Wohlenschwil
                      #+ent_Wohlenschwil
                      , data = factor_pop)
summary(lm_Wohlenschwil)
plot(lm_Wohlenschwil)

lm_Würenlingen <- lm(pop_Würenlingen~rent_Würenlingen
                     #+ health_Würenlingen
                     #+ edu_Würenlingen
                     #+ ent_Würenlingen
                     #+edu_Baden
                     +ent_Untersiggenthal
                     #+health_Obersiggenthal
                       , data =factor_pop)
summary(lm_Würenlingen)
vif(lm_Würenlingen)
plot(lm_Würenlingen)

lm_Würenlos <- lm(pop_Würenlos~house_Würenlos
                  #+health_Würenlos
                  #+edu_Würenlos
                  #+ ent_Würenlos
                  + health_Neuenhof
                  #+ health_Killwangen
                  , data =factor_pop)
summary(lm_Würenlos)
vif(lm_Würenlos)
plot(lm_Würenlos)

lm_Ehrendingen <- lm(pop_Ehrendingen~rent_Ehrendingen
                     #+house_Ehrendingen
                     #+ health_Ehrendingen
                     #+ edu_Ehrendingen
                     #+ ent_Ehrendingen
                     + health_Ennetbaden
                     #+ health_Baden
                     , data =factor_pop)
summary(lm_Ehrendingen)
vif(lm_Ehrendingen)
plot(lm_Ehrendingen)




corr_matrix_commune("Baden",factor_pop)
corr_matrix_factor("house",factor_pop)
fpop_factor_plot("house")
pair_plot("Baden")


