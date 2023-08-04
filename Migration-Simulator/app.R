# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(rstudioapi)
library(tidyverse) 
library(dplyr)
library(lintr) # code linting
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(cowplot)
library(readr)
library(readxl)
library(shiny)
library(magick)

#canton_geo = read_sf("Boundary_Data/g2k23.shp")

#country_geo= read_sf("Boundary_Data/g2l23.shp")
politics <- read_excel("politicalparties.xlsx",col_names = F)
colnames(politics) <- c("Gemeinde","party_num", "party", "percentage") #name the columns
politics <- filter(politics, party_num %in% c(1,2,3,4,31,13))# Filter for only these parties: FDP, CVP, SP, SVP, GLP, GPS
politics_improved <- data.frame(matrix(ncol = 7, nrow = 0)) #create a new database, empty
colnames(politics_improved) <- c("Gemeinde", "FDP", "CVP", "SP", "SVP", "GPS", "GLP")
#for each municipality, give percentage for each party
for (i in 1:nrow(politics)){ 
  if (i%%6==0) {#Since there are 6 parties, this will happen once for each municipality
    row <- c(politics[i,1])#name of municipality
    for (party in 1:6) {#go thru each party in 1 municipality
      row <- append(row, politics[i+party-6,4])#Add the % for each party to the row
    }
    politics_improved[nrow(politics_improved)+1,] <- row #Add the new row to the new database
  }
}

municipality_geo <- read_sf("Boundary_Data/g2g23.shp") #Shape data for the municipality boundaries
gemeinden_baden <- read_excel("2021_Gemeinden.xlsx") %>% #List and Population of each municipality in Baden
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
gemeinden_coords <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))
map500 <- raster("Maps/Baden500_excess.tif")%>% 
  as("SpatialPixelsDataFrame") %>% #Turn into dataframe to plot into ggplot
  as.data.frame() %>%
  rename(relief = `Baden500_excess`)

legend_options <- c("GPS %", "SP %", "GLP %", "CVP %", "FDP %", "SVP %")
party_options <- c(6,4,7,3,2,5)
baden_map <- function(visual_data, fill_data, legend)
{ 
  print("map loading")
  ggplot(
    data=visual_data, #Database, where the visual data
    aes(fill=fill_data) #Variable that dictates fill of each municipality
  ) +
    #Map Background
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          alpha=relief 
          #since fill is already used for the data, alpha values are used to paint the map
          #eventually, either a 2nd fill will be attempted with workarounds, or plot transitioned to leaflet instead of ggplot
      ),
    ) +
    scale_alpha(#How to fill the map
      name = "",
      range = c(0.9,0),
      guide = F
    ) +
    geom_sf( #Create the municipality Boundaries
      data = gemeinden_coords,
      color = "transparent",
      size = 0.5) +
    
    scale_fill_viridis(#Set a custom fill for the data to be visualized
      option = "magma",
      alpha = 0.6, #make them slightly transparent to see map background
      begin = 0.1,
      end = 0.9,
      direction = -1,
      name = legend
    ) +
    #remove visual clutter
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    ) 
 
}
baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Gesamtbevölkerung")
ui <- fluidPage(
  titlePanel(h1(strong("Migration Simulator"))),
  sidebarLayout(position = "right",
                sidebarPanel(h4(strong("Slider:")),
                    sliderInput(inputId = "politics",
                    label = "Political Orientation",
                    min = 0,
                    max = 6,
                    value = 0)
                ),

                #Main Part of Website (displaying map & Slider values)
                mainPanel(("The following map shows the population of the region Baden. 
                          By moving the slider on the right the map will display the popularity of different political parties."),
                          br("1=GPS, 2=SP, 3=GLP, 4=CVP, 5=FDP, 6=SVP"),
                           plotOutput("map"),
                          tableOutput("values")
                )
  )
)
# Define server logic ----
server <- function(input, output) {
  sliderValues <- reactive({
    data.frame(#getting data about input of slider
      Name = c("politics"),
      Value = as.character(c(input$politics)),
      stringsAsFactors = FALSE)
    
  })

  output$values <- renderTable({#Table displays the input values of slider
    sliderValues()
  })

  
  
  output$map <- renderPlot({
    visual_option <- (sliderValues()[1,2])
    numeric_visual_option <- as.numeric(visual_option)
    #Visualize all the maps
    if (visual_option==0) {
      baden_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Gesamtbevölkerung")
    }
    else{if(visual_option==1){baden_map(politics_improved, politics_improved$GPS,"GPS %")}
    else{if(visual_option==2){baden_map(politics_improved, politics_improved$SP,"SP %")}
    else{if(visual_option==3){baden_map(politics_improved, politics_improved$GLP,"GLP %")}
    else{if(visual_option==4){baden_map(politics_improved, politics_improved$CVP,"CVP %")}
    else{if(visual_option==5){baden_map(politics_improved, politics_improved$FDP,"FDP %")}
    else{baden_map(politics_improved, politics_improved$SVP,"SVP %")}
      }}}}}

    }) 
}
# Run the app ----
shinyApp(ui = ui, server = server)
