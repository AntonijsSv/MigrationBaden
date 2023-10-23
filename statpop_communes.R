library(tidyverse) 
library(dplyr)
library(readr)
library(readxl)
library(xlsx)

communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDE=`Gmd-Nr.`) #mutate adds a new column to gemeinden_baden, 
#this mutation just makes a new column called GMDNR, it takes and copies the data from the Gmd-Nr.


yrs <- c(2022:2011)




for (i in 1:length(yrs)) {
  #aka for the amount of files
  print(yrs[i])#print which year is being worked on
  if (yrs[i] > 2020) {
    filename <- paste0("analysis/statpop/GMDE/STATPOP",yrs[i],"_GMDE.csv")
    pop_yr <- read_delim(filename, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
    baden_pop <- left_join(pop_yr, communes_baden, by="GMDE") %>%
      filter(!is.na(Gemeinde))%>%
      dplyr::select(starts_with("G")|contains("B"))
    if (i == 1) {
      #The goal is to create a new data frame containing data per commune, so we start the df by setting it equal to the first year data
      statpop_baden <- baden_pop
    }
    else {
      baden_pop %>%
        dplyr::select(-c(Gesamtbevölkerung,`Gmd-Nr.`,Gemeinde,))
      statpop_baden <- full_join(statpop_baden, baden_pop, by="GMDE") 
      #here you join the data from the years together
      #fulljoin(... by ="reli") will join rows together with the same RELI
      #If one dataset doesnt have a certain RELI, then it will stil, create a new row, but put NA for whereever there isnt data
      #e.g. in 2012 theres suddenly data for reli abc, which doesnt exist already in the data set, it will create a new row
      #this new row will have all the data from 2012 and for all the other years, there will be nothing
    }
  }
  else {
    if (yrs[i] == 2020) {
      filename <- paste0("analysis/statpop/GMDE/STATPOP",yrs[i],"_GMDE.csv")
      pop_yr <- read_delim(filename, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)%>%
        mutate(GMDE = `GDENR`)
      baden_pop <- left_join(pop_yr, communes_baden, by="GMDE") %>%
        filter(!is.na(Gemeinde))%>%
        dplyr::select(starts_with("G")|contains("B"))%>%
        dplyr::select(-c(Gesamtbevölkerung,`Gmd-Nr.`,GDENR,Gemeinde,))
      statpop_baden <- full_join(statpop_baden, baden_pop, by="GMDE")
    }
    else {
      filename <- paste0("analysis/statpop/GMDE/STATPOP",yrs[i],"_GMDE.csv")
      pop_yr <- read_csv(filename)%>%
        mutate(GMDE = `GDENR`)
      baden_pop <- left_join(pop_yr, communes_baden, by="GMDE") %>%
        filter(!is.na(Gemeinde))%>%
        dplyr::select(starts_with("G")|contains("B"))%>%
        dplyr::select(-c(Gesamtbevölkerung,`Gmd-Nr.`,GDENR,Gemeinde,))
      statpop_baden <- full_join(statpop_baden, baden_pop, by="GMDE")
    }
  }
}
write.csv(statpop_baden,"analysis/STATPOP_Communes.csv")

population_baden <- mutate(statpop_baden, Gemeinde = Gemeinde.x) %>%
  dplyr::select(c(GMDE, Gemeinde) | contains("BTOT") )
