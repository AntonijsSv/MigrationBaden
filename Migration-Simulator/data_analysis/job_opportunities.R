library(readxl)
library(dplyr)

data1 <- read_excel("Migration-Simulator/main_data/job_opportunities.xlsx")
data2 <- read_excel("Migration-Simulator/main_data/population.xlsx")

job <- list()
population <- list()
job_percent <- list()
population_percent <- list()
x <- 2019

for (i in 1:nrow(data1)) {
  while (x > 2011) {
    

  value1 <- data1$x[i]  
  value2 <- data2$x[i] 
  
  # Add values to vectors
  job <- c(job, value1)
  population <- c(population, value2)
  
  for (i in length(job)) {
    dif <- job[i+1] - job[i]
    percent <- dif/job[i]
    
    job_percent <- c(job_percent, percent)
  }
  
  for (i in length(population)) {
    dif <- population[i+1] - population[i]
    percent <- dif/population[i]
    
    population_percent <- c(population_percent, percent)
  }
  
  for(i in length(job_percent)){
    percent <- population_percent[i]*job_percent[i]*0.174242424 #value from Forms data
    percent_list <- c(percent_list, percent)
  }
  
  average <- mean(percent_list)
  average_list <- c(average_list, average)
  }  
}

final <- mean(average_list)


