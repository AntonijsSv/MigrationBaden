#Cleaning Housing Data into 1 file
#Take data from rightadvisor 
#Use Swiss stat data to extrapolate to older years -/-> inaccurate
#Code in order to get rent data into 1 file
library(tidyverse) 
library(dplyr)
library(xlsx)

library(readxl)

years = c("2021","2020","2019","2018","2017","2016","2015","2014","2013","2012")
rent <- data.frame(matrix(ncol = 16, nrow = 0))
for (i in 1:length(years)){
  rent_data <- read_excel("C:/Users/colte/Downloads/rent.xlsx",
                          sheet = years[i])
  rent_data <- rent_data[24,]
  
  rent_year = years[i] %>%
  append(rent_data)
  print(rent_year)
  rent[nrow(rent)+1,] <- rent_year
}
  
rent
write.xlsx(rent, "C:/Users/colte/Downloads/rent_R.xlsx", sheetName = "Sheet1", 
           col.names = F, row.names = F, append = F)


Houseprices <- read_excel("Houseprices.xlsx", sheet = "Sheet1")

house <- read_excel("Houseprices.xlsx", sheet = "Sheet2")
rent <- read_excel("Houseprices.xlsx", sheet = "Sheet3")



for (i in 1:nrow(Houseprices)) {
  print("i")
  print(i)
  rate <- Houseprices[i,14]
  rate <- as.numeric(rate)+2

  for (j in 3:19){
    print(j)
    Houseprices[i, 15+j-3] <- house[j,rate]
  }
  
}

write.xlsx(Houseprices, "C:/Users/colte/Downloads/Houseprices_R.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = F)

Houseprices <- read_excel("Houseprices_R.xlsx", sheet = "Sheet1")

percent_product <- function(values) {
  cumprod <- 1
  #print(values)
  for (j in 1:ncol(values)) {
    j <- as.numeric(values[1,j])/100 + 1
    #print(j)
    #print("j")
    cumprod <- cumprod*j
  }
  prod <- (cumprod-1)*100
  return(prod)
}
  

for (i in 1:26) {
  Houseprices[i,32] <- Houseprices[i,31]
  Houseprices[i,33] <- percent_product(Houseprices[i,31:30])
  Houseprices[i,34] <- percent_product(Houseprices[i,31:27])
  Houseprices[i,35] <- percent_product(Houseprices[i,31:16])
  Houseprices[i,36] <- as.numeric(Houseprices[i,32])-Houseprices[i,8]
  Houseprices[i,37] <- as.numeric(Houseprices[i,33])-Houseprices[i,9]
  Houseprices[i,38] <- as.numeric(Houseprices[i,34])-Houseprices[i,10]
  Houseprices[i,39] <- as.numeric(Houseprices[i,35])-Houseprices[i,11]
}
colnames(Houseprices[36:39]) <- c("delta 1Q", "delta 2Q","delta 1 Yr", "delta 5Yrs")
