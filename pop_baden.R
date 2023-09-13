library(tidyverse)
library(dplyr)#for data frame manipulation
library(readr) #work with csv files
library(readxl) #read excel files
#library(xlsx)

communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) 
#mutate adds a new column to communes_baden, 
#this mutation just makes a new column called GMDE, it takes and copies the data from the Gmd-Nr.


#Get the coordinate range of municipalites
#This range is the reason why STATENT data cannot be used by itself and gemeinden_baden also needs to be used
years <- c(21:13)
#List of all years

for (i in 1:length(years)) {
  #Go through each year 
  print(years[i])
  yr <- years[i]
  #The below code just opens up the population file, due to the different file types, if else is required
  if (yr >= 18) {
    pop_yr <- paste0("analysis/population/pop",years[i],".xlsx")
    pop_yr <- read_excel(pop_yr)
    
  }
  else {
    pop_yr <- paste0("analysis/population/pop",years[i],".xls")
    pop_yr <- read_excel(pop_yr)
    
  }
  
  pop_yr <- pop_yr[-1:-2,]
  #remove the first 2 columns, they do not contain any data
  colnames(pop_yr)[1] <- "GMDE" #Rename the first column to GMDE, to make the following code easier to understand
  pop_names <- slice(pop_yr, 1:4) #take the top rows with the names of the data
  pop_yr <- filter(pop_yr, GMDE >= 4021, GMDE <= 4049)
  # 4021 and 4049 are commune numbers, this includes all communes of bezirk baden
  # We slice the top 4 rows in order to keep them after filtering, as otherwise they would be deleted
  
if (years[i] == 21) {
  #if its the first year in the data set aka 2021, then you create a new data set that for now is just pop data for that year
  #Later, data from other years will be added onto pop_baden
  pop_baden <- pop_yr #Adds 1st dataset to baden df
  baden_names <- pop_names #The same thing goes for the name
}
else {
  pop_baden <- full_join(pop_baden, pop_yr, by="GMDE")
  #here you join the data from the years together
  #fulljoin(... by ="reli") will join rows together with the same RELI <- coordinates
  #Occasionally, certain hectares will suddenly appear or disappear from the selection, this leads to NA for certain years
  pop_names <- subset(pop_names, select = -GMDE)
  #we remove the GMDE column, as this column "merges with the one from 2021
  #Simply put, GMDE for each year is removed thru full_join(), meaning it also has to be removed from the names
  baden_names <- cbind(baden_names,pop_names)
  #cbind "attaches" the names to each other, making a data frame consisting of just the names of all the data
  #this will later be added on top of the actual data to finalize the data frame
}
}
colnames(baden_names) <- colnames(pop_baden)
#set the column names equal between the data and the names
pop_baden <- rbind(baden_names,pop_baden)
#Add the names on top of the data to create the final data frame

write.csv(pop_baden,"analysis/population/Population_Baden_2021_2013.csv")
