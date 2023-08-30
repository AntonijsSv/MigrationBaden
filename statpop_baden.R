library(tidyverse) 
library(dplyr)
library(readr)
library(readxl)
library(xlsx)

communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #mutate adds a new column to gemeinden_baden, 
#this mutation just makes a new column called GMDNR, it takes and copies the data from the Gmd-Nr.

#combines the above datasets and groups them by GMDE,
#for all municipalities outside Baden, this will create NA, which is then filtered out

#Get the coordinate range of municipalites
#This range is the reason why statpop data is used and not just gemeinden_baden



statpop <- c("analysis/statpop/STATPOP2021.csv",
             "analysis/statpop/STATPOP2020.csv",
             "analysis/statpop/STATPOP2019.csv",
             "analysis/statpop/STATPOP2018.csv",
             "analysis/statpop/STATPOP2017.csv",
             "analysis/statpop/STATPOP2016.csv",
             "analysis/statpop/STATPOP2015.csv",
             "analysis/statpop/STATPOP2014.csv",
             "analysis/statpop/STATPOP2013.csv",
             "analysis/statpop/STATPOP2012.csv",
             "analysis/statpop/STATPOP2011.csv")

# Names of files

years <- c(2021:2011)
# Amount of years

for (i in 1:length(statpop)) {#For the amount of items in the list statpop
  #aka for the amount of files
  #i will take on different values each time it goes thru the loop, in this case 1 to the number of files (10)
  #each time this loop occurs, i will increase by 1 
  print(years[i])#will take the i-th item in the list, which will be the year of the file
  if (years[i] >=2020) {#if the year is 2020 or later
    NOLOC_yr <- paste0("analysis/statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
    #paste0 puts these things together without adding any spaces inbetween
    
    coords_baden <- read_delim(NOLOC_yr, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)%>%
      select(E_KOORD,N_KOORD,RELI,GDENR)%>%
      mutate(GMDE=`GDENR`) 
    #open statpop data, which contains commune number data, and select its coordinates and number
    #reli is just E_koord + N_koord
    
    baden_data <- left_join(coords_baden,communes_baden, by="GMDE") %>% #Now you add all the data from gemeinden_baden to geodata
      #you join it by GMDNR meaning rows with the same GMDNR will put together into 1 row, if GMDNR doesnt exist in gemeinden_baden
      #then all the cells with data from gemeinden_baden will be NA as they dont exist
      filter(!is.na(Gemeinde))
    # Gemeinde is a column in gemeinden_baden, as all GMDNRs that are not in Baden will not have data for Gemeinde, it will be NA
    # By filtering by this column with !is.na, it will check whether it is na, if it is, it will delete that row, if it isnt NA, it will filter it and keep it
    # Basically, you take a column that only exists in gemienden_baden which will be NA for communes not in Baden then you filter them out
    
    e_range <- range(baden_data$E_KOORD)
    n_range <- range(baden_data$N_KOORD)
    #get the range of x and y coordinates
    
    statpop_yr <- read_delim(statpop[i], 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
      filter(E_KOORD %in% (e_range[1]:e_range[2]), N_KOORD %in% (n_range[1]:n_range[2]))
    #open the statpop data and then filter the coordinates that they are only within the range of coordinates in Baden
    
    
    
    if (years[i] == 2021) {
    #if its the first year in the data set aka 2021, then you create a new dataset that for now is just statpop data for that year
      statpop_baden <- statpop_yr #Adds 1st dataset to baden df
    }
    else {#for every other year
      select(statpop_yr, -c(X_KOORD,Y_KOORD,N_KOORD,E_KOORD)) #-c() will remove those columns
      #Removes Coordinates, as they stay constant throughout the years, so that the data is cleaner to look through
      statpop_baden <- full_join(statpop_baden, statpop_yr, by="RELI") 
      #here you join the data from the years together
      #fulljoin(... by ="reli") will join rows together with the same RELI
      #If one dataset doesnt have a certain RELI, then it will stil, create a new row, but put NA for whereever there isnt data
      #e.g. in 2012 theres suddenly data for reli abc, which doesnt exist already in the data set, it will create a new row
      #this new row will have all the data from 2012 and for all the other years, there will be nothing
    }
      
  }
  else {#this does the same thing as the code above, but for all the years before 2020, the data were in csv with commas instead of
    #...semicolons, so this is practically the same code but with slight differences to account for the differences in the data
    NOLOC_yr <- paste0("analysis/statpop/NOLOC/STATPOP",years[i],"_NOLOC.csv")
    
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
  

}


