library(readxl)
library(dplyr)

data1 <- read_excel("Migration-Simulator/main_data/services.xlsx")
data2 <- read_excel("Migration-Simulator/main_data/population.xlsx")

services <- list()
population <- list()
services_percent <- list()
population_percent <- list()
x <- 2019

for (i in 1:nrow(data1)) {
  while (x > 2011) {
    
    
    value1 <- data1$x[i]  
    value2 <- data2$x[i] 
    
    # Add values to vectors
    services <- c(services, value1)
    population <- c(population, value2)
    
    for (i in length(services)) {
      dif <- services[i+1] - services[i]
      percent <- dif/services[i]
      
      services_percent <- c(services_percent, percent)
    }
    
    for (i in length(population)) {
      dif <- population[i+1] - population[i]
      percent <- dif/population[i]
      
      population_percent <- c(population_percent, percent)
    }
    
    for(i in length(services_percent)){
      percent <- population_percent[i]*services_percent[i]*0.174242424 #value from Forms data
      percent_list <- c(percent_list, percent)
    }
    
    average <- mean(percent_list)
    average_list <- c(average_list, average)
    x <- x-1
  }  
}

final <- mean(average_list)


