library(tidyverse) 

library(dplyr)

library(readr)

library(readxl)


communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  
  mutate(GMDE=`Gmd-Nr.`) #give the municipality number column the same name as in coords_gmde





years <- c(2023:2013)



for (i in 1:length(years)) {
  
  print(years[i])
  
    
    NOLOC_yr <- ("analysis/statpop/NOLOC/STATPOP2021_NOLOC.csv")
    
    
    coords_baden <- read_delim(NOLOC_yr, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)%>%
      
      dplyr::select(E_KOORD,N_KOORD,RELI,GDENR)%>%
      
      mutate(GMDE=`GDENR`) 
    
    
    baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>%
      
      filter(!is.na(Gemeinde))
    
    
    e_range <- range(baden_data$E_KOORD)
    
    n_range <- range(baden_data$N_KOORD)
    
    
    ov_yr <- paste0("OeV/OeV_Haltestellen_ARE_",years[i], ".csv")
    
    ov_yr <- read_delim(ov_yr, delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE) #%>%
      ov_yr <- filter(ov_yr,X_Koord %in% (e_range[1]:e_range[2]), Y_Koord %in% (n_range[1]:n_range[2]))
    
    
    if (years[i] == 2023) {
      
      ov_baden <- ov_yr #Adds 1st dataset to baden df
      
    }
    
    else {
      
      #dplyr::select(ov_yr, -c(Name)) #Removes Coordinates, as they stay constant throughout the years
      
      ov_baden <- full_join(ov_baden, ov_yr, by= "Name") 
      
      #Appends the new dataset to baden df, by joining by RELI, data belonging to each ha is put on the same row
      
    }}
    

write.csv(ov_baden,"OV_Baden_2023_2012.csv")