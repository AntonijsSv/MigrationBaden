library(ggplot2)
library(dplyr)
library(readxl)
library(readr)

pop_data_gmd <- read_delim("STATPOP2021_NOLOC.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  select(E_KOORD,N_KOORD,GMDE)

gemeinden <- read_excel("2021_Gemeinden.xlsx") %>%
  mutate(GMDE=`Gmd-Nr.`)

gmd_data <- left_join(pop_data_gmd,gemeinden, by="GMDE") %>%
  filter(!is.na(Gemeinde))
x_range <- range(gmd_data$E_KOORD)
y_range <- range(gmd_data$N_KOORD)

pop_data <- read_csv("PopDataperHectare.csv")%>%
  filter(E_KOORD %in% (x_range[1]:x_range[2]), N_KOORD %in% (y_range[1]:y_range[2]))

gmd_tile <- ggplot(pop_data, aes(x=E_KOORD,y= N_KOORD)) +
  geom_tile(aes(fill=cut(B21BTOT,c(1,4,7,16,41,121,Inf)))) +
  scale_fill_manual(
    values=c("(1,4]"="#ffffb2",
             "(4,7]"="#fdd976",
             "(7,16]"="#feb243",
             "(16,41]"="#fd8d3c",
             "(41,121]"="#f03b20",
             "(121,Inf]"="#bd0026"), 
    labels=c("1-3","4-6","7-15","16-40","41-120",">120"),
    name="population per ha",
    na.value = "green") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        #rect = element_blank()
  )
gmd_tile
