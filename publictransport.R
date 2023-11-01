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
    
    
    y_range <- range(baden_data$E_KOORD)
    
    x_range <- range(baden_data$N_KOORD)
    
    
    ov_yr <- paste0("OeV/OeV_Haltestellen_ARE_",years[i], ".csv")
    
    ov_yr <- read_delim(ov_yr, delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE) #%>%
      ov_yr <- filter(ov_yr,between(X_Koord,x_range[1],x_range[2]),between(Y_Koord,y_range[1],y_range[2]))
      
    ov_names <- colnames(ov_yr)
    for (j in 1:length(ov_names)) {
      if (ov_names[j] != "Name") {
        colnames(ov_yr)[j] <- paste0(ov_names[j],"_",years[i])
      }
    }
    
    if (years[i] == 2023) {
      
      ov_baden <- ov_yr #Adds 1st dataset to baden df
      
    }
    
    else {
      
      #dplyr::select(ov_yr, -c(Name)) #Removes Coordinates, as they stay constant throughout the years
      
      ov_baden <- full_join(ov_baden, ov_yr, by= "Name") 
      
      #Appends the new dataset to baden df, by joining by RELI, data belonging to each ha is put on the same row
      
    }}
    

write.csv(ov_baden,"OV_Baden_2023_2012_1.csv")