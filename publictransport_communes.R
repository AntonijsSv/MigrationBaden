library(tidyverse) 
library(dplyr)
library(readr)
library(stringr)
library(readxl)


ov <- read_csv("OV_Baden_2023_2012.csv")
communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx")

communes <- communes_baden$Gemeinde #We just want the names, nothing else

input_umlaut <- c("Mägenwil","Würenlos","Würenlingen","Künten")
output_umlaut <- c("genwil","renlos","renlingen","nten")

for (i in 1:length(communes)) { #Since the files put umlauts as ?, we have to detect commune names with the part of the name that comes after the commune
  communes[i] <- gsub("\\(|\\)", "", communes[i]) #remove parenthesis
  for (j in  1:length(input_umlaut)){ #Check if its a problem commune and change it
    if (str_detect(communes[i],input_umlaut[j])) {
      communes[i] <- output_umlaut[j]
    }
  }
}

years = c(2023:2013)

ov_communes <- data.frame(Communes = communes_baden$Gemeinde,amount_stops=rep(0,26))
#Create a new data frame in which we will put all the data

for (i in 1:length(years)){ #For each year, we want a column for buses and one for trains
    train_yr <- paste0("train_",years[i])
    bus_yr <- paste0("bus_",years[i])
    ov_communes[[train_yr]] <- rep(0,26) #rep(0,26) creates a list of 26 zeroes -> a column of just zero
    ov_communes[[bus_yr]] <- rep(0,26)
}

obersiggenthal <- c("Rieden","Kirchdorf","Nussbaumen")#Obersiggenthal is divided into 3 sub communes

for (stops in 1:nrow(ov)) {# Go through each bus stop/train connection
  stop_name = as.character(ov[stops,5])#Column 5 contains the names of the stops
  
  for (commune in 1:length(communes)) {
  #Go through each commune and check whether the stop is in the commune (the name of the commune will be in the name of the stop)
    if  (str_detect(stop_name,communes[commune])) { #Detects whether the name of a commune is in the bus stop name, returns true/false
      ov_communes[commune,2] = ov_communes[commune,2]+1 #the 2nd column is a total counter of stops within a commune
      
      for (yr in 1:length(years)) { 
        #Make the column names for the annual counter
        train_yr <- paste0("train_",years[yr]) 
        bus_yr <- paste0("bus_",years[yr])
        
        #Gets the value of trains/buses/trams in a given year from the original dataframe
        #We assume NA = 0 as most stops with lacking information are bus stops that have been renamed, hence "doesn't exist" anymore
        train <- as.numeric(ov[stops,10*yr-3])
        if (is.na(train)) {train = 0}
        bus <- as.numeric(ov[stops,10*yr-2])
        if (is.na(bus)) {bus = 0}
        
        #Add up the amount of bus and train connections
        ov_communes[commune,train_yr] <- ov_communes[commune,train_yr]+train
        ov_communes[commune,bus_yr] <- ov_communes[commune,bus_yr]+bus
      }
    }
  }
  for (os_commune in 1:length(obersiggenthal)) {
  #Since Obers. is split up into subcommunes, the name Obersiggenthal doesn't appear, we have to check for sub communes
    if  (str_detect(stop_name,obersiggenthal[os_commune])) { #Detects whether the name of a commune is in the bus stop name, returns true/false
      ov_communes[17,2] = ov_communes[17,2]+1 #the 2nd column is a total counter of stops within a commune, the 17th row is obersiggenthal
      
      for (yr in 1:length(years)) { 
        #Make the column names for the annual counter
        train_yr <- paste0("train_",years[yr]) 
        bus_yr <- paste0("bus_",years[yr])
        
        #Gets the value of trains/buses/trams in a given year from the original dataframe
        #We assume NA = 0 as most stops with lacking information are bus stops that have been renamed, hence "doesn't exist" anymore
        train <- as.numeric(ov[stops,10*yr-3])
        if (is.na(train)) {train = 0}
        bus <- as.numeric(ov[stops,10*yr-2])
        if (is.na(bus)) {bus = 0}
        
        #Add up the amount of bus and train connections
        ov_communes[17,train_yr] <- ov_communes[17,train_yr]+train
        ov_communes[17,bus_yr] <- ov_communes[17,bus_yr]+bus
      }
    }
  }
}



write.csv(ov_communes,"Oev/OV_Baden_communes_2023_2013.csv")
