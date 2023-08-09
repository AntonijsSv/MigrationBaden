library(tidyverse) 
library(dplyr)
library(readr)
library(readxl)
library(xlsx)

communes_baden <- read_excel("2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #give the municipality number column the same name as in coords_gmde

#combines the above datasets and groups them by GMDE,
#for all municipalities outside Baden, this will create NA, which is then filtered out

#Get the coordinate range of municipalites
#This range is the reason why statpop data is used and not just gemeinden_baden



statpop <- c("statpop/STATPOP2021.csv",
             "statpop/STATPOP2020.csv",
             "statpop/STATPOP2019.csv",
             "statpop/STATPOP2018.csv",
             "statpop/STATPOP2017.csv",
             "statpop/STATPOP2016.csv",
             "statpop/STATPOP2015.csv",
             "statpop/STATPOP2014.csv",
             "statpop/STATPOP2013.csv",
             "statpop/STATPOP2012.csv",
             "statpop/STATPOP2011.csv")

years <- c(2021:2011)

for (i in 1:length(statpop)) {
  print(years[i])
  if (years[i] >=2020) {
    NOLOC_yr <- paste0("statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
    
    coords_baden <- read_delim(NOLOC_yr, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)%>%
      select(E_KOORD,N_KOORD,RELI,GDENR)%>%
      mutate(GMDE=`GDENR`) 
    
    baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>%
      filter(!is.na(Gemeinde))
    
    e_range <- range(baden_data$E_KOORD)
    n_range <- range(baden_data$N_KOORD)
    
    
    statpop_yr <- read_delim(statpop[i], 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
      filter(E_KOORD %in% (e_range[1]:e_range[2]), N_KOORD %in% (n_range[1]:n_range[2]))
    
    if (years[i] == 2021) {
      statpop_baden <- statpop_yr #Adds 1st dataset to baden df
    }
    else {
      select(statpop_yr, -c(X_KOORD,Y_KOORD,N_KOORD,E_KOORD)) #Removes Coordinates, as they stay constant throughout the years
      statpop_baden <- full_join(statpop_baden, statpop_yr, by="RELI") 
      #Appends the new dataset to baden df, by joining by RELI, data belonging to each ha is put on the same row
    }
      
  }
  else {
    NOLOC_yr <- paste0("statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
    
    coords_baden <- read_delim(NOLOC_yr, show_col_types = FALSE)%>%
      select(E_KOORD,N_KOORD,RELI,GDENR)%>%
      mutate(GMDE=`GDENR`)
    
    baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>%
      filter(!is.na(Gemeinde))
    
    e_range <- range(baden_data$E_KOORD)
    n_range <- range(baden_data$N_KOORD)
    
    
    statpop_yr <- read_delim(statpop[i],show_col_types = FALSE) %>%
      filter(E_KOORD %in% (e_range[1]:e_range[2]), N_KOORD %in% (n_range[1]:n_range[2]))%>%
      select(-c(X_KOORD,Y_KOORD,E_KOORD,N_KOORD))
    statpop_baden <- full_join(statpop_baden, statpop_yr, by="RELI")
  }
  
  #needs to be altered to be 1 df with all years
}


