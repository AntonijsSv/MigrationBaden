# Libraries ----
library(tidyverse) 
library(readxl)
#library(dplyr) included in tidy verse
#library(readr) included in tidy verse
#library(stringr) included in tidy verse

# Files & Values ----
gws_file <- read_csv("analysis/GWS_21_13_wCommunes.csv") %>%
  dplyr::select(-c(E_KOORD_2020,N_KOORD_2020,Y_KOORD_2020,X_KOORD_2020,E_KOORD_2021,N_KOORD_2021))%>%
  dplyr::select(-contains("JAHR"))%>%
  dplyr::select(c(GMDNAME,RELI) | contains("_20"))

gws_file[is.na(gws_file)] <- 0

statent_file <- read_csv("analysis/STATENT_20_11_wCommunes.csv")%>%
  relocate(GMDNAME)%>%
  dplyr::select(-contains("JAHR"))%>%
  dplyr::select(c(GMDNAME,RELI) | contains("_20"))
statent_file[is.na(statent_file)] <- 0

statpop_file <- read_csv("analysis/STATPOP_21_11_wCommunes.csv")


communes_baden <- read_excel("analysis/2021_Gemeinden.xlsx")$Gemeinde  #We just want the names, nothing else
communes_baden <- gsub("\\(AG)|", "",communes_baden)


# Function hell ----
add_columns <- function(df,column_list,suffix) {
  #' adds columns onto a df with a suffix
  #' df is a data frame
  #' column list is a list of all the column names
  #' suffix is a string to add to the column names -> e.g. yrs
 for (col in column_list) {
   new_col <- paste0(col,suffix)
   df[[new_col]] <- 0
 } 
  return(df)
}

create_df <- function(df, communes,suffix) {
  #'creates a data frame with all the columns (empty) to fill in later
  #'df is the dataframe you want to use
  #'communes is a list of all communes
  #'yrs is a list of the years to go through
  df_communes <- data.frame("Communes" = communes)
  columns <- c()
  for (col in 1:ncol(df)) {
    if (str_detect(as.character(colnames(df)[col]),suffix)) {
      columns <- c(columns,colnames(df)[col])
    }
  }
  df_communes <- add_columns(df_communes,columns,"")
  print("df created")
  return(df_communes)
}

#gws_0 <- create_df(gws_file,communes_baden,years_gws)
#statent_0 <- create_df(statent_file,communes_baden,years_ent)
statpop_0 <- create_df(statpop_file,communes_baden,"BTOT")

add_to_commune <- function(commune, 
                           row, 
                           data, 
                           output,
                           start_col_data, 
                           start_col_output) {
  column_output <- start_col_output
  for (column_data in start_col_data:ncol(data)){
      value <- data[row, column_data]
      output[commune, column_output] <- output[commune, column_output] + value
      column_output <- column_output + 1
  }
  
  return(output)
}

find_ha_in_commune <- function(commune, 
                               ha_in_commune, 
                               data, 
                               output,
                               start_col_data, 
                               start_col_output) {
  #' commune is a string of the commune
  #' ha_in_commune a list of boolean T/F values whether a ha is in a commune
  #' data data frame
  #' output data frame
  #' start_col_data and start_col_df are numbers indicating where to start analysing data -> avoid names, coordinates etc.
  print(commune)
  for (ha in 1:length(ha_in_commune)) {
    if (ha_in_commune[ha]) {
      print(paste("row",ha))
      
      #output <- add_to_commune(commune, ha, 
      #                         data, output, 
      #                         start_col_data, start_col_output)
      value <- data[ha,start_col_data:ncol(data)]
      value[is.na(value)] <- 0
      output[commune,start_col_output:ncol(output)] <- 
        output[commune,start_col_output:ncol(output)] + value
    }
  }
  return(output)
}

communizator <- function(data, 
                         communes_list,
                         start_col_data,
                         start_col_output,
                         suffix) {
  #'takes ha data and turns it into a data frame per commune
  #'data is the data frame with all data
  #'communes is a list of all the communes
  #'yrs are all the years for which there is data
  #'ha_commune_col,is a number, its the nth column where the communes are given
  #'col_start is when the data (numbers) in the data frame actually begins (after coordinates and names, etc.)
  df <- create_df(data,
                  communes_list,
                  suffix)
  for (commune in 1:length(communes_list)) {
    ha_in_commune <- grepl(communes_list[commune],
                           gws_file$GMDNAME)
    print(paste("commune,list",
                communes_list[commune]))
    df <- find_ha_in_commune(commune,
                             ha_in_commune,
                             data, 
                             df,
                             start_col_data, 
                             start_col_output)
  }
  return(df)
  #for (ha in 1:nrow(data)) {
  #  print(ha)
  #  column_df<- 2 #since some columns in data are not data but some extra junk, we have to avoid them
  #  for (columns in col_start:ncol(data)) {
  #     #They dont start at the same place
  #    if (str_detect(colnames(data)[columns],"_20")&&!str_detect(colnames(data)[columns],"JAHR")) {
  #      commune <- data[ha,ha_commune_col]
  #      #print(commune)
  #      x <- commune_detect(df,
  #                          commune,communes_list,
  #                          ha,column_df,
  #                          data[ha,columns])
  #      df[ha,column_df] <- x
  #      column_df <- column_df+1
  #    }
  #  }
  #}
  

}

# Make the new files  ----
#gws <- communizator(gws_file,
#                    communes_baden,
#                    3,
#                    2)

#statent <- communizator(statent_file,
#                        communes_baden,
#                        3,
#                        2,
#                        "_20")

# Make Files -----

#write.csv(gws,"analysis/GWS_Communes.csv")

