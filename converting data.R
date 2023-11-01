# baden 

library(readxl)
rent_prices <- read_excel("rent prices.xlsx")
View(rent_prices)


for (i in 5:nrow(rent_prices)) {
house_2023 = as.numeric(rent_prices[i, 5])
apartement_2023 = as.numeric(rent_prices[i, 10])
rent <- mean(c(house_2023,apartement_2023), na.rm = TRUE)
print (rent)
rent_newyear <- rent * rent_prices[i,13]
rent_prices[i, 22] <- rent_newyear

  for (l in 13:21) {
    rent_newyear <- rent * rent_prices[i, l]
    rent_prices[i, l+ 9] <- rent_newyear
  }
}

write.csv(rent_prices, "rent_prices.csv")


