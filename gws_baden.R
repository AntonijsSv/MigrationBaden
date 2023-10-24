library(tidyverse) 
library(dplyr)
library(readr)
library(readxl)
library(xlsx)
library(ggplot2)

communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #give the municipality number column the same name as in coords_gmde


years <- c(2021:2013)

add_yr <- function(df,exemption,yr) {
#'adds the suffix "_[yr]" to all columns but the exeption
#'df is the dataframe
#'exemption is a column you want unchanged -> column name, must be a string
#'yr is the year or anything for that matter to discern it from other columns
  for (column in 1:ncol(df)) {
    if(colnames(df)[column]!=exemption){
      colnames(df)[column] = paste0(colnames(df)[column],"_",yr)
    }
  }
  return(df)
}

for (i in 1:length(years)) {
  print(years[i])
  if (years[i] >=2020) {
    NOLOC_yr <- paste0("analysis/statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
    
    
    coords_baden <- read_delim(NOLOC_yr, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)%>%
      dplyr::select(E_KOORD,N_KOORD,RELI,GDENR)%>%
      mutate(GMDE=`GDENR`) 
    
    baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>%
      filter(!is.na(Gemeinde))
    
    e_range <- range(baden_data$E_KOORD)
    n_range <- range(baden_data$N_KOORD)
    
    gws_yr <- paste0("analysis/GWS/GWS",years[i],"_HA.csv")
    
    gws_yr <- read_delim(gws_yr, 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
      filter(E_KOORD %in% (e_range[1]:e_range[2]), N_KOORD %in% (n_range[1]:n_range[2]))
    
    if (years[i] == 2021) {
      gws_baden <- gws_yr%>%#Adds 1st dataset to baden df
      add_yr("RELI",2021)
    }
    else {
      gws_yr %>%
      dplyr::select(-c(X_KOORD,Y_KOORD,N_KOORD,E_KOORD)) #Removes Coordinates, as they stay constant throughout the years
      gws_yr <- add_yr(gws_yr,"RELI",years[i])
      gws_baden <- full_join(gws_baden, gws_yr, by="RELI") 
      #Appends the new dataset to baden df, by joining by RELI, data belonging to each ha is put on the same row
    }
    
  }
  else {
    NOLOC_yr <- paste0("analysis/statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
    
    coords_baden <- read_delim(NOLOC_yr, show_col_types = FALSE)%>%
      dplyr::select(E_KOORD,N_KOORD,RELI,GDENR)%>%
      mutate(GMDE=`GDENR`)
    
    baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>%
      filter(!is.na(Gemeinde))
    
    e_range <- range(baden_data$E_KOORD)
    n_range <- range(baden_data$N_KOORD)
    
    gws_yr <- paste0("analysis/GWS/GWS",years[i],"_HA.csv")
    gws_yr <- read_delim(gws_yr, show_col_types = FALSE) %>%
      filter(E_KOORD %in% (e_range[1]:e_range[2]), N_KOORD %in% (n_range[1]:n_range[2]))%>%
      dplyr::select(-c(X_KOORD,Y_KOORD,E_KOORD,N_KOORD))%>%#Removes Coordinates, as they stay constant throughout the years
      add_yr("RELI",years[i])
    gws_baden <- full_join(gws_baden, gws_yr, by="RELI")
  }
  
}

write.csv(gws_baden,"GWS_Baden_2021_2013.csv")

