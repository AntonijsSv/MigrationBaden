#Cleaning Housing Data into 1 file
#Take data from rightadvisor 
#Use Swiss stat data to extrapolate to older years -/-> inaccurate
#Code in order to get rent data into 1 file
library(tidyverse) 
library(dplyr)
library(xlsx)
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
