library(tidyverse) 
library(dplyr)
library(readr)
library(readxl)
library(xlsx)

communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #mutate adds a new column to communes_baden, 
#this mutation just makes a new column called GMDE, it takes and copies the data from the Gmd-Nr.


years <- c(2020:2011) #List of all the years for which we have statent data

for (i in 1:length(years)) {#For the amount of items in the list years
  #aka for the amount of files
  print(years[i])#will take the i-th item in the list, which will be the year of the file
  NOLOC_yr <- paste0("analysis/statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
  #paste0 puts these things together without adding any spaces inbetween
  #create the filename for the file that contains coordinates and commune numbers, this data will then be chopped down...
  #...to only contain coordinates for communes in Baden, these coordinates will then be used to chop down the actual data set
  
    
  if (years[i] >= 2020) {
    delim_type <- ";"
  }
  else {
    delim_type <- ","
  }
  #The datasets are csv files that use different delimiters
  #aka what symbol do they use to say that theres a new cell
  coords_baden <- read_delim(NOLOC_yr, delim = delim_type, escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)%>%
    #opens csv file using the delimiter
    dplyr::select(E_KOORD,N_KOORD,RELI,GDENR)%>%
    #only select coordinates and commune number
    mutate(GMDE=`GDENR`) 
  #mutate makes a new column called GMDE thats a copy of GDENR
    
  baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>%
    #Now you add all the data from gemeinden_baden to geodata
    #you join it by GMDNR meaning rows with the same GMDNR will put together into 1 row, if GMDNR doesnt exist in gemeinden_baden
    #then all the cells with data from gemeinden_baden will be NA as they dont exist
    filter(!is.na(Gemeinde))
    # Gemeinde is a column in gemeinden_baden, as all GMDNRs that are not in Baden will not have data for Gemeinde, it will be NA
    # By filtering by this column with !is.na, it will check whether it is na, if it is, it will delete that row, if it isnt NA, it will filter it and keep it
    # Basically, you take a column that only exists in gemienden_baden which will be NA for communes not in Baden then you filter them out
    
  e_range <- range(baden_data$E_KOORD)
  n_range <- range(baden_data$N_KOORD)
  #get the range of x and y coordinates
  
  statent_yr <- paste0("analysis/STATENT/STATENT_N08_",years[i],".csv")
  #create the filename containing statent data
  statent_yr <- read_delim(statent_yr, 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
    filter(E_KOORD %in% (e_range[1]:e_range[2]), N_KOORD %in% (n_range[1]:n_range[2]))%>%
    #filter out all coordinated that are in the predefined range, this trims the data to only Baden
    dplyr::select(-c(ERHJAHR,PUBJAHR))
    #remove these columns
  
  
  #Here the big file is created
  statent_col <- colnames(statent_yr)
  #I take all the names of the columns and put it in a new list
  for (j in 1:ncol(statent_yr)) {
    #for all the columns with data, this code will add the year to the name of the columns
    #we dont want to change the names of columns with coordinates (1-3) as we use them to join datasets between the years, so they have to be the same
    if (statent_col[j]!= "RELI") {
        colnames(statent_yr)[j] <- paste0(statent_col[j],"_",years[i])
        #The name of the j-th column is its original name + the year of the data
        }
  }
  if (years[i] == 2020) {
    #if its the first year in the data set aka 2020, then you create a new dataset that for now is just statpop data for that year
    statent_baden <- statent_yr #Adds 1st dataset to baden
  }
  else {
    dplyr::select(statent_yr, -c(paste0("N_KOORD","_",years[i]),
                                 paste0("E_KOORD","_",years[i])))
    #Removes Coordinates, as they stay constant throughout the years and years as they are already in the col names
    statent_baden <- full_join(statent_baden, statent_yr, by="RELI") 
    #here you join the data from the years together
    #fulljoin(... by ="reli") will join rows together with the same RELI
    #If one dataset doesnt have a certain RELI, then it will stil, create a new row, but put NA for whereever there isnt data
    #e.g. in 2012 theres suddenly data for reli abc, which doesnt exist already in the data set, it will create a new row
    #this new row will have all the data from 2012 and for all the other years, there will be nothing
    #This is why we started changing the names of columns starting from 4, to ensure that RELI would stay the same
  }
    
}



write.csv(statent_baden,"STATENT_Baden_2020_2012.csv") #Make the new file with all the data
