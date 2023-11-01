library("ggplot2")
library(readr)
rent_prices <- read_csv("rent_prices.csv")
Population_Baden_2021_2013 <- read_csv("analysis/population/Population_Baden_2021_2013.csv")
library(readxl)
population_baden <- read_excel("analysis/population_baden.xlsx")
View(population_baden)
library(tidyverse)

x <- c(2013: 2021)
x2<- c(2013:2022)
storagepop<- data.frame(years = x)
storagerent<- data.frame(years = x2)



for (t in 5:30) {
  
  
  y <- c()
  y2<- c()
  for (s in 1:length(x)) {
    popyr = as.numeric(Population_Baden_2021_2013[t,42*s-38])
    y<- c(y, popyr)
  }
  
  y[7]<- Population_Baden_2021_2013[t,260]
  y[8]<- Population_Baden_2021_2013[t,306]
  y[9]<- Population_Baden_2021_2013[t,343]
  
  rent_prices1 <- c(247)
  
  for (s in 2:length(x2)) {
    rentprices = as.numeric (rent_prices[t,s + 21])
    rent_prices1<- c(rent_prices1, rentprices)
  }
  
  
  storagepop[ncol(storagepop) + 1] <- c(y)
  storagerent[ncol(storagerent)+1] <- c(rent_prices1)
  
}

ggplot(storagepop, aes(x=years, y=storagepop[,3])) +
  geom_point() + 
  geom_point(data=storagerent, aes(x=years,y=storagerent[,3]))

rent_tibble<- storagerent$V2
rent_vector<- as.numeric(t(rent_tibble))#as.numeric() turns it into a numeric vector
pop_vector <- as.numeric(population_baden[2, 3:12]) #as.numeric() turns it into a numeric vector
cor(rent_vector,pop_vector) #cor only works with numeric vectors
