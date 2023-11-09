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
library(patchwork)
library(ggnewscale)
library(shinyjs)

# Files ----
#Shape data for the municipality boundaries
municipality_geo <- read_sf("Boundary_Data/g2g23.shp")

#List and Population of each municipality in Baden in 2021
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

# Join both files, this is done using the column GMDNR
#for all municipalities outside Baden, this will create NA, which is then filtered out
commune_general_info <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))

#purely geographical information for each commune
commune_geo <- dplyr::select(commune_general_info,c(Gemeinde,GMDNR,geometry))
commune_geo$Gemeinde <- gsub("\\(AG)|", "",commune_geo$Gemeinde)%>%
  str_trim()
#remove (AG) suffix to some communes 
commune <- commune_general_info$GMDNAME
#purely the population of each commune from 2022 to 2011
population <- read_xlsx("analysis/population_baden.xlsx")%>%
  dplyr::select(-GMDE)

pop <- t(population)
colnames(pop) <- pop[1,]
pop <- as.tibble(pop[-1,])

gws <- read_csv("analysis/GWS_Communes.csv") %>%
  dplyr::select(-"...1")
gws_geo <- left_join(commune_geo,gws,by=join_by(Gemeinde==Communes))

statent <- read_xlsx("analysis/STATENT_Communes.xlsx")
statent_geo <- left_join(commune_geo,statent,by=join_by(Gemeinde))

ov <- read_csv("OV_Baden_communes_2023_2013.csv")

# ggplot map ----
map500 <- raster("Maps/Baden500_excess.tif")%>% 
  as("SpatialPixelsDataFrame") %>% #Change the file type to convert into a dataframe, ggplot only accepts  data frames
  as.data.frame() %>%
  rename(relief = `Baden500_excess`)

map_colours <- c("white",# Generic land
                 "#CDE6BE",# wooded
                 "red",# Unknown, not on this map
                 "#F6DBA4",# urban areas
                 "#ECA8C8",# cantonal borders
                 "transparent",# cantonal names
                 "red",# Unknown, not on this map
                 "#FFF582",# side roads
                 "#F5AE95",# main roads
                 "orange",# highways
                 "#DD1A19",# train lines
                 "#D2EEFF",# lakes
                 "#0088D0",# River names
                 "#0088D0",# Rivers
                 "#ECA8C8",# National borders
                 "black",# Place names
                 "black",# Airports
                 "black",# Golf courses
                 "black",# Road Outlines
                 "black",# Urban area outlines
                 "black"# Place dot 
)
place_names <- rep("transparent",21)
place_names[16] <- "black"


baden_commune_map <- function(visual_data,fill_data,legend) 
{
  #'visual data is the data frame with the data
  #'fill data is the the column in the visual data data frame that will be visualized
  #'legend is the title of the legend
  ggplot() + 
    #The map background
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=map_colours[relief]
          #since fill is already used for the data, alpha values are used to paint the map
          #eventually, either a 2nd fill will be attempted with workarounds, or plot transitioned to leaflet instead of ggplot
      )
    ) +
    scale_fill_identity() +
    
    new_scale_fill() +
    
    #visualization of commune data
    geom_sf( #Create the municiplality Boundaries
      data = visual_data,
      aes(fill=fill_data),
      color = "black",
      size = 0.5 ) +
    
    scale_fill_viridis(#Set a custom fill for the data to be visualised
      option = "magma",
      alpha = 0.6, #make them slightly transparent to see map background
      begin = 0.1,
      end = 0.9,
      direction = -1,
      name = legend
    ) +
    
    new_scale_fill() +
    
    #Add a new layer to overlay place names above the data
    geom_raster(
      data = map500,
      inherit.aes = FALSE,
      aes(x,y,
          fill=place_names[relief]
      )
    ) +
    scale_fill_identity() +
    
    #Theme aesthetics
    theme_minimal()+
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ggplot ----




dataset <- function(data,value) {
  rownames(data) <- data[,1]
  data <-  dplyr::select(data,contains(value)) #only select values to plot
  colnames(data) <- gsub("(.*?)_(\\d+)", "\\2", colnames(data)) #Finds the year in the colnames and extracts the year
  data <- t(data) 
  data <- cbind(rownames(data),data.frame(data, row.names = NULL))
  colnames(data)[1] <- "Years"
  return(data)
  
}

gws_tot <- (dataset(as.data.frame(gws),"GTOT"))
statent_tot <- (dataset(as.data.frame(statent),"B08T"))




ui <- fluidPage(                                                           
  titlePanel(h1(strong("Migration Simulation"))),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "choice",
                  label = h4(strong("Select a municipality:")),
                  choices = c("Baden",
                              "Bellikon",
                              "Bergdietikon",
                              "Birmenstorf",
                              "Ennetbaden",
                              "Fislisbach",
                              "Freienwil",
                              "Gebenstorf",
                              "Killwangen",
                              "Kuente",
                              "Maegenwil",
                              "Mellingen",
                              "Neuenhof",
                              "Niederrohrdorf",
                              "Oberrohrdorf",
                              "Obersiggenthal",
                              "Remetschwil",
                              "Spreitenbach",
                              "Stetten", 
                              "Turgi",
                              "Untersiggenthal",
                              "Wettingen",
                              "Wohlenschwil",
                              "Wuerenlingen",
                              "Wuerenlos",
                              "Ehrendingen"
                  )
      ),
      h4(strong("Migration Factors:")),
      useShinyjs(),
      sliderInput(inputId = "health",
                  label = "Health Services",
                  min = -100,
                  max = 100,
                  value = 0),
      sliderInput(inputId = "house",
                  label = "House/Rental Prices",
                  min = -100,
                  max = 100,
                  value = 0),
      sliderInput(inputId = "jobs",
                  label = "Job Opportunities",
                  min = -100,
                  max = 100,
                  value = 0),
      actionButton("go", "GO"),
      tableOutput("values")
    ),                
                #Main Part of Website (displaying map & Slider values)
                mainPanel(("The following map shows the population of the municipalities in the region Baden. By chosing a municipality in the drop down menu and moving the sliders on the left the migration factors can be adjusted for a certain municipality and the map will display the change in population of all municipalities."),
                          plotOutput("map"),
                          
                )
  )
)

# Define server logic ----
server <- function(input, output) {
  observeEvent(input$go, {
    showNotification("GO button pressed.", type = "message", duration = 15000)
  })
  
  output$selected_option <- renderText({
    paste("You selected:", input$choice)
  })
  
  # Reactive expression to create a data frame of all input values
  sliderValues <- reactive({
    health_value <- input$health/100
    house_value <- input$house/100
    jobs_value <- input$jobs/100
    data.frame(
      Name = c("Health services:", "House/Rental Prices:", "Job Opportunities:"),
      Value = c(health_value, house_value, jobs_value),
      stringsAsFactors = FALSE
  )
})
  
  output$values <- renderTable({
    sliderValues()
  })  
  
  
  output$map <- renderPlot({
    visual_option <- (sliderValues()[1,2])
    numeric_visual_option <- as.numeric(visual_option)
    
    options(
      shiny.reactlog = TRUE,
      shiny.reactlog_interval = 1000
    )
    #Visualize all the maps
    baden_commune_map(gemeinden_coords, gemeinden_coords$Gesamtbevölkerung, "Population")


    
  }, width = 900, height = 600) 
}
# Run the app ----
shinyApp(ui = ui, server = server)