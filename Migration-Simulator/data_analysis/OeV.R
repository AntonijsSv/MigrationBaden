library(readxl)
library(dplyr)

data1 <- read_excel("Migration-Simulator/main_data/OeV.xlsx")
data2 <- read_excel("Migration-Simulator/main_data/population.xlsx")

colnames(data1)
colnames(data2)

# Initialize lists and variables
job <- list()
population <- list()
job_percent <- list()
population_percent <- list()
percent_list <- c()  # Initialize as an empty numeric vector
average_list <- c()  # Initialize as an empty numeric vector

x <- 2019

# Loop over the years
while (x > 2011) {
  for (i in 1:nrow(data1)) {
    x <- str(x)
    value1 <- data1[i, x]
    value2 <- data2[i, x]
    
    # Add values to vectors
    job <- c(job, value1)
    population <- c(population, value2)
    
    x <- int(x)
    # Calculate job and population percent changes
    if (i > 1) {
      job_dif <- job[i] - job[i - 1]
      population_dif <- population[i] - population[i - 1]
      job_percent <- job_dif / job[i - 1]
      population_percent <- population_dif / population[i - 1]
      
      # Append to percent lists
      job_percent <- c(job_percent, job_percent)
      population_percent <- c(population_percent, population_percent)
      
      # Calculate percent and append to the percent_list
      percent <- population_percent[i] * job_percent[i] * 0.174242424
      percent_list <- c(percent_list, percent)
    }
  }
  
  # Calculate the average for the current year
  average <- mean(percent_list)
  average_list <- c(average_list, average)
  x <- x - 1
}

# Calculate the final average
final <- mean(average_list)
