library(tidyverse) 
library(dplyr)
library(readr)
library(readxl)
library(xlsx)

communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GDENR=`Gmd-Nr.`)%>% #mutate adds a new column to gemeinden_baden, 
  dplyr::select(Gemeinde, GDENR)
#this mutation just makes a new column called GMDNR, it takes and copies the data from the Gmd-Nr.


yrs <- c(2021:2011)

for (i in 1:length(yrs)) {
  #aka for the amount of files
  print(yrs[i])#print which year is being worked on
  filename <- paste0("analysis/STATENT/GMDE/STATENT_N08_GMDE_",yrs[i],".csv")
  ent_yr <- read_delim(filename, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) %>%
    dplyr::select(-contains("JAHR"))
  baden_ent <- left_join(communes_baden, ent_yr, by="GDENR") %>%
    filter(!is.na(Gemeinde))
  colnames(baden_ent)[3:ncol(baden_ent)] <- paste0(colnames(baden_ent)[3:ncol(baden_ent)],"_",yrs[i])
  if (i == 1) {
    #The goal is to create a new data frame containing data per commune, so we start the df by setting it equal to the first year data
    statent_baden <- baden_ent
  }
  else {
    baden_ent <- dplyr::select(baden_ent,-contains("Gemeinde"))
    statent_baden <- full_join(statent_baden, baden_ent, by="GDENR") 
      #here you join the data from the years together
      #fulljoin(... by ="reli") will join rows together with the same RELI
      #If one dataset doesnt have a certain RELI, then it will stil, create a new row, but put NA for whereever there isnt data
      #e.g. in 2012 theres suddenly data for reli abc, which doesnt exist already in the data set, it will create a new row
      #this new row will have all the data from 2012 and for all the other years, there will be nothing
  }
}
write.csv(statent_baden,"analysis/STATENT_Communes.csv")

population_baden <- mutate(statpop_baden, Gemeinde = Gemeinde.x) %>%
  dplyr::select(c(GMDE, Gemeinde) | contains("BTOT") )

write.csv(population_baden,"analysis/population_baden.csv")
