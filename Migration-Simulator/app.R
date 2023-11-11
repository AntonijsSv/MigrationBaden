# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#Libraries ----
library(rstudioapi)
library(tidyverse) 
library(sf) # spatial data handling
library(raster)
library(viridis) # viridis color scale
library(readxl)
library(shiny)
library(magick)
library(patchwork)
library(ggnewscale)
library(ggarchery)
library(shinyjs)

# Files ----
factor_pop <- read.csv("final_dataset.csv")
sf_conversion <- function(df_file,x,y){
  #'df_file is the input file, it requires an x and y coordinate column
  #'ensure that the x and y columns do not have NAs: df %>% filter(!is.na(column))
  #'x and y NEED TO BE STRINGS, should be the name of the column with the coordinates
  #'If these columns do not have names, use: colnames(df)[x] <- "E_KOORD"
  #'commune_geo should be an sf file of Baden -> boundaries
  sf_file <- st_as_sf(df_file, coords=(c(x,y)))
  #Take popdata and turn it from a basic data frame into a sf file, give it coordinates
  sf_file$centre <- st_centroid(sf_file)
  hectare_size <- 100
  sf_file <- st_buffer(sf_file$centre,0.5*hectare_size,endCapStyle="SQUARE")#%>%
  #  st_simplify()
  #pop_data_sf <- st_make_grid(pop_data_sf,hectare_size)
  #?st_simplify
  sf_file <- st_set_crs(sf_file,st_crs(commune_geo))
  #set the same coordinated system as gemeinden_coords
  sf_file <- st_join(sf_file, commune_geo)
  #Every hectare (point) in pop_data_sf is checked in which commune (mutlipolygon) its located
  #every hectare is assigned to a commune
  sf_file <- filter(sf_file,!is.na("Gemeinde"))
  #remove all points/hectares not inside a commune
  return(sf_file)
} 
# Function that takes hectare data and converts it to sf data and assigns each hectare to a commune

#Shape data for all municipality boundaries
municipality_geo <- read_sf("Boundary_Data/g2g23.shp")
#List and Population of each municipality in Baden in 2021
gemeinden_baden <- read_excel("analysis/2021_Gemeinden.xlsx") %>% 
  mutate(GMDNR=`Gmd-Nr.`) #Rename certain column to be the same in both files

commune_general_info <- left_join(municipality_geo,gemeinden_baden, by="GMDNR") %>% 
  filter(!is.na(Gemeinde))
#Joins geometry data with baden general info, filters out all communes not in Baden district

#Extracts commune geometry and commune name data from general info
commune_geo <- dplyr::select(commune_general_info,c(Gemeinde,GMDNR,geometry))
commune_geo$Gemeinde <- gsub("\\(AG)|", "",commune_geo$Gemeinde)%>%
  str_trim()
#remove (AG) suffix to some communes 

communes <- gemeinden_baden$Gemeinde
communes <- gsub("\\(AG)|", "",communes)%>%
  str_trim()
#Create a list of communes

communes<-communes[-5]
communes <- c(communes,"Ehrendingen")
#Rearrange the communes list alphabetically as Ehrendingen was at the end originally 

#population of each commune from 2022 to 2011
population <- read_xlsx("analysis/population_baden.xlsx")
pop <- dplyr::select(population,"GMDE","population" = "2022")

population_geo <- read_csv("analysis/STATPOP_21_11_wCommunes.csv")%>%
  dplyr::select(-...1)%>%
  mutate(x = E_KOORD, y = N_KOORD)

pop_geo <- sf_conversion(population_geo,"x","y")%>%
  dplyr::select(-Gemeinde.y)
colnames(pop_geo)[1] <- "Gemeinde"
ha_geo <- dplyr::select(pop_geo,Gemeinde,E_KOORD,N_KOORD,geometry,B21BTOT)

commune_geo <- left_join(commune_geo,pop, by= join_by(GMDNR==GMDE))

pop_df <- as.data.frame(population$"2021"[-1])
colnames(pop_df) <- "old_pop"
pop_df$new_pop <- pop_df$old_pop

pop_analysis <- function(pop) {
  #' Function to add population change info to a pop data frame
  pop <- mutate(pop,pop_change= pop$new_pop-pop$old_pop)
  #pop[[pop_change]] <- pop[[new_pop]]-pop$old_pop
  pop <- mutate(pop,pop_percentage=pop$new_pop/pop$old_pop)
  #pop[[pop_percentage]] <- pop[[new_pop]]/pop$old_pop
  pop <- mutate(pop,net_migration_percent=pop$pop_percentage-1-0.01050109*0.8)
  #pop$net_migration_percent <- pop[[pop_percentage]]-1-0.01050109*0.8
  #net population growth in the entire district*the amount of that originating from birth/death
  #pop[[net_migration]] <- pop$old_pop*pop[[net_migration_percent]]
  pop <- mutate(pop,net_migration=pop$old_pop*pop$net_migration_percent)
  
  
  rownames(pop) <- communes
  return (pop)
}

pop_df <- pop_analysis(pop_df)

fpop <- filter(factor_pop, Years == 2021)

factors <- colnames(factor_pop)[-c(1:27)]
factor_change <- data.frame(factors = factors, change = 1)
factor_change$factors <- gsub("\\(AG)|", "",factor_change$factors)
factor_change$factors <- gsub("\\..AG.|", "",factor_change$factors)

factor_change <- as.data.frame(t(factor_change))
colnames(factor_change) <- factor_change[1,]
factor_change <- dplyr::select(factor_change, starts_with(c("Year","house","rent","ent_","edu","health")))



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

# Coefficients ----
coef_Baden <- data.frame(coefficient=c("Intercept","rent_baden","edu_baden","ent_baden","health_baden"),
                         value = c(18722.953506, 11.350421, 4.829288, -1.240632, 4.364191))
coef_Bellikon <- data.frame(coefficient=c("Intercept","rent_bellikon","edu_bellikon","ent_bellikon","health_bellikon"),
                            value = c(1473.5045931, 0.3991652, 2.3630931, 0.3799376, -1.2387387))
coef_Bergdietikon <- data.frame(coefficient=c("Intercept","rent_bergdietikon","edu_bergdietikon","health_bergdietikon","ent_bergdietikon","ent_bellikon"),
                                value = c(2605.392354, 3.194327, -13.046932, 13.687470, 1.937724, -6.995531))
coef_Birmenstorf <- data.frame(coefficient=c("Intercept","house_birmenstorf","ent_birmenstorf","health_birmenstorf","edu_birmenstorf"),
                               value = c(-2509.1699083, 0.1036378, 29.0945234, -17.4955843, -6.7901265))
coef_Ehrendingen <- data.frame(coefficient=c("Intercept","rent_ehrendingen","health_ehrendingen","edu_ehrendingen","ent_ehrendingen","health_ennetbaden"),
                               value = c(4803.4280544, 2.3406851, -37.5065624, 8.3782970, 0.3075315, 14.5713066))
coef_Ennetbaden <- data.frame(coefficient=c("Intercept","rent_ennetbaden","edu_baden","health_ennetbaden","ent_ennetbaden","edu_ennetbaden"),
                              value = c(1976.895519, 2.548093, 4.237226, -5.628923, 3.327639, 3.178744 ))
coef_Fislisbach <- data.frame(coefficient=c("Intercept","house_fislisbach","health_fislisbach","edu_fislisbach","ent_fislisbach"),
                              value = c(3824.6934918,0.1686995,-20.7352791,6.7736144,4.4623245))
coef_Freienwil <- data.frame(coefficient=c("Intercept","house_freienwil","edu_freienwil","ent_freienwil","health_freienwil"),
                             value = c(390.65135613, 0.09613579, 0, -1.64552364, 28.73733984))
coef_Gebenstorf <- data.frame(coefficient=c("Intercept","house_gebenstorf","ent_gebenstorf","health_gebenstorf","edu_gebenstorf"),
                              value = c(1832.8666469, 0.4895858, 1.0662318, -5.1684264, 6.3912439))
coef_Killwangen <- data.frame(coefficient=c("Intercept","house_killwangen","ent_killwangen","edu_killwangen","health_killwangen"),
                              value = c(878.63922189, 0.08506498, 6.05574741, -18.20455843, -10.33643870))
coef_Künten <- data.frame(coefficient=c("Intercept","house_künten","health_künten","edu_künten","ent_künten"),
                          value = c(928.60548084, 0.13144222, 8.47340091, -5.01405734, -0.01651525))
coef_Mägenwil <- data.frame(coefficient=c("Intercept","rent_mägenwil","ent_mägenwil","edu_mellingen","edu_mägenwil","health_mägenwil"),
                            value = c(1582.738044, 1.702261, 1.333401, 3.911492, 2.778947, 6.970707))
coef_Mellingen <- data.frame(coefficient=c("Intercept","rent_mellingen","health_mellingen","edu_mellingen","ent_mellingen","ent_fislisbach"),
                             value = c(274.7192201, 12.8040883, 0.5836928, -48.0007417, 15.0362766, 0.4539605))
coef_Neuenhof <- data.frame(coefficient=c("Intercept","house_neuenhof","health_neuenhof","edu_neuenhof","ent_neuenhof"),
                            value = c(4320.1430750, 0.4630323, -7.9489663, -22.6455862, 5.1012512))
coef_Niederrohrdorf <- data.frame(coefficient=c("Intercept","house_niederrohrdorf","edu_oberrohrdorf","ent_niederrohrdorf","edu_niederrohrdorf","health_niederrohrdorf"),
                                  value = c(1877.5472727, 0.4474443, -43.3997678, 1.8055217, -2.6150562, -16.6376079 ))
coef_Oberrohrdorf <- data.frame(coefficient=c("Intercept","house_oberrohrdorf","edu_oberrohrdorf","ent_oberrohrdorf","health_oberrohrdorf"),
                                value = c(3084.7303926, 0.1529742, -0.9588283, 6.1889287, 3.2997615))
coef_Obersiggenthal <- data.frame(coefficient=c("Intercept","house_obersiggenthal","health_obersiggenthal","ent_obersiggenthal","edu_obersiggenthal"),
                                  value = c(4.770200e+03, 9.132275e-02, 2.717372e+01, 5.456018e+00,1.199151e+01))
coef_Remetschwil <- data.frame(coefficient=c("Intercept","rent_remetschwil","edu_remetschwil","health_remetschwil","ent_remetschwil"),
                               value = c(2632.5060709, 0.1523921, 0.2619645, 20.6831222, -5.4645576))
coef_Spreitenbach <- data.frame(coefficient=c("Intercept","rent_spreitenbach","health_spreitenbach","ent_spreitenbach","edu_spreitenbach"),
                                value = c(10131.000818, 11.699141, 56.819847, -3.912515, 25.383187))
coef_Turgi <- data.frame(coefficient=c("Intercept","rent_turgi","ent_turgi","health_turgi","edu_turgi"),
                         value = c(2376.939820, 0.032677, 2.044775, 2.796493, 3.028180))
coef_Stetten <- data.frame(coefficient=c("Intercept","rent_stetten","health_stetten","edu_stetten","ent_stetten"),
                           value = c(2001.469, 7.369, -2.398, 8.594, -3.416))
coef_Untersiggenthal <- data.frame(coefficient=c("Intercept","house_untersiggenthal","ent_untersiggenthal","health_untersiggenthal","edu_untersiggenthal"),
                                   value = c(4620.33048275, 0.09753149, 6.37650787, 14.58098721, -8.36437175 ))
coef_Wettingen <- data.frame(coefficient=c("Intercept","rent_wettingen","edu_wettingen","health_wettingen","ent_wettingen"),
                             value = c(12772.693753, 4.406298, 35.816860, 6.977471, 2.944182))
coef_Wohlenschwil <- data.frame(coefficient=c("Intercept","house_wohlenschwil","health_wohlenschwil","edu_wohlenschwil","ent_wohlenschwil"),
                                value = c(1113.81743263, 0.09659737, -2.93676094, -7.05608405, -0.65940067))
coef_Würenlingen <- data.frame(coefficient=c("Intercept","rent_würenlingen","health_würenlingen","edu_würenlingen","ent_würenlingen","ent_Untersiggenthal"),
                               value = c(2650.178783, 10.211383, 3.371687, -12.522055, -1.714890, 6.819109))
coef_Würenlos <- data.frame(coefficient=c("Intercept","house_würenlos","health_würenlos","edu_würenlos","ent_würenlos","health_Neuenhof"),
                            value = c(2544.8913199, 0.5344122, 0.7655624, -9.5892315, 3.3145121, -40.5820143))


coefficients <- paste0("coef_",communes)


# Simulation ----

slider_to_factor <- function(slider_input) {
  #' slider_input is a df containing all interactions with user
  #' data is the 
  #Extract data 
  f_change <- factor_change
  df <- as.data.frame(t(slider_input))
  colnames(df) <- df[1,]
  commune <- dplyr::select(df,contains("Commune"))[2,]
  health <- dplyr::select(df,contains("health"))[2,]
  house <- dplyr::select(df,contains("house"))[2,]
  rent <- dplyr::select(df,contains("rent"))[2,]
  ent <- dplyr::select(df,contains("Job"))[2,]
  edu <- dplyr::select(df,contains("Education"))[2,]
  cat(commune,health,house,rent,ent,edu)
  
  f_change <- dplyr::select(f_change,ends_with(paste0("_",commune)))
  
  factor_name <- c("house","rent","ent","edu","health")
  factor <- c(house,rent,ent,edu,health)
  f_change[1,] <- commune 
  for (i in 1:5) {
    f <- paste0(factor_name[i],"_",commune)
    #print(f)
    #print(f_change[[f]])
    
    factor_i <- as.numeric(factor[i])
    #print(factor[i])
    #print(factor_i)
    f_change[[f]][2] <- factor_i
  }
  #view(f_change)
  return(f_change)
}

factor_to_pop <- function(factor_change_df,factor_value_df,pop) {
  #'chose THE COLUMN IN YOUR POP DF YOU WANT CHANGED
  print("start factor to pop")
  commune <- factor_change_df[1,1]
  
  c <- which(communes==commune)
  for (i in 1:length(communes)) {
    coef_commune <- paste0("coef_",communes[i])
    influential_factors <- get(coef_commune)$coefficient #includes intercept
    #print(c(influential_factors,"factors"))
    influential_values <- get(coef_commune)$value
    #print(c(influential_values,"value"))
    #cat(influential_factors,influential_values,"influential factors, values")
    pop[i] <- influential_values[1] #set population to intercept
    for (f in 2:length(influential_factors)) {
      coef <- influential_factors[f]
      #print(c(coef,"coef"))
      factor_change <- 1
      if (c==i) {
        factor_change <- as.numeric(dplyr::select(factor_change_df,starts_with(coef))[2,])
      }
      factor_value <- as.numeric(dplyr::select(factor_value_df,starts_with(coef))[1,])
      #print(c(factor_value,"value"))
      #print(c(factor_change,"change"))
      #print(c(pop[c],influential_values[i],factor_change*factor_value))
      pop[i] <- pop[i]+influential_values[f]*factor_change*factor_value
    }
    
    pop[i]<- pop[i]*runif(1,min=0.95,max=1.05)
  }
  
  
  return(pop)
}

emigration <- function(pop) {
  #'pop is pop_df
  #'limit refers to min amount of people you want to indicate an arrow with
  arrow_data <- data.frame(x1 = 0,
                           x2 = 0,
                           y1 = 0,
                           y2 = 0,
                           people = 0)
  for (commune in 1:length(communes)) {
    net_m <- as.integer(pop$net_migration)[commune]
    #print(net_m)
    c_geo <- filter(commune_geo, Gemeinde == communes[commune])
    neighbour_communes_distance <- st_is_within_distance(c_geo,commune_geo,dist=2000)[[1]]
    #print(neighbour_communes_distance)
    neighbour_communes_geo <- commune_geo[neighbour_communes_distance,]%>%
      setdiff(c_geo)
    neighbour_communes <- (neighbour_communes_geo$Gemeinde)
    #print(neighbour_communes)
    tot_pop_neighbour_commune <- sum(neighbour_communes_geo$population)
    
    #print(neighbour_communes)
    #print(neighbour_communes_geo)
    for (neighbour in 1:length(neighbour_communes)) {
      n_geo <- st_centroid(neighbour_communes_geo[neighbour,])
      migration <- n_geo$population/tot_pop_neighbour_commune*net_m
      #People will travel to larger communes rather than small ones
      
      if (net_m < 0) { #people move out
        start_point <- st_coordinates(st_centroid(c_geo))
        end_point <- st_coordinates(n_geo)
      }
      else { #people move in
        start_point <- st_coordinates(n_geo)
        end_point <- st_coordinates(st_centroid(c_geo))
      }
      start_x <- start_point[1]
      start_y <- start_point[2]
      end_x <- end_point[1]
      end_y <- end_point[2]
      arrow_data <- rbind(arrow_data,c(start_x,end_x,start_y,end_y,migration))
    }
  }
  arrow_data <- arrow_data[-1,]
  return(arrow_data)
}

# Function to plot arrows on the map
arrow_plot <- function (plot, arrow_data, limit) {
  #arrow data needs to be this format: x1,x2,y1,y2,people
  #limit refers to amount of people is required before an arrow is drawn
  arrow_data <- filter(arrow_data,abs(people) > limit)
  p <- plot + new_scale_colour() +
    geom_segment(data=arrow_data, 
                 aes(x=x1,y=y1,xend=x2,yend=y2,color=people),
                 arrow = arrow(angle=40,length=unit(0.3,"cm"),ends="last",type="closed"),
                 alpha = 0.8,
                 linewidth = 1.2,
                 position = position_attractsegment(start_shave = 0.1, 
                                                    end_shave = 0.1))+
    scale_color_viridis(
      name = paste("People Migrating, at least", limit, "people")
    )
  return(p)
}

simulate <- function(slider_input,pop,legend,arrow_limit) {
  factor_change <- slider_to_factor(slider_input)
  pop$new_pop <- factor_to_pop(factor_change,fpop,pop$new_pop)
  pop <- pop_analysis(pop)
  plot_df <- commune_geo
  plot_df$new_pop <- pop$new_pop
  p <- baden_commune_map(plot_df,plot_df$new_pop,legend)
  coords <- (emigration(pop))
  final_plot <- arrow_plot(p, coords, arrow_limit)
  return(final_plot)
}

# UI ----

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
      sliderInput(inputId = "edu",
                  label = "Education",
                  min = -100,
                  max = 100,
                  value = 0),
      actionButton("go", "GO")
    ),
    #Main Part of Website (displaying map & Slider values)
    mainPanel(("The following map shows the population of the municipalities in the region Baden. By chosing a municipality in the drop-down menu and moving the sliders on the left the migration factors can be increased/decreased in percentage (%) for a certain municipality. Then the GO button needs to pressed in order for the map to display the change in population of all municipalities."),
              plotOutput("map")
              
    )
  ))

# Server ----
server <- function(input, output) {
  # Create a reactiveVal to store the commune
  selected_commune <- reactiveVal("Baden")
  
  # Create reactive values to store the slider values
  sliderValues <- reactiveValues(
    health_value = 1,
    house_value = 1,
    jobs_value = 1,
    edu_value = 1,
    choice_value = "Baden"
  )
  
  observeEvent(input$go, {
    showNotification("Processing", type = "message", duration = 10)
    # Update the reactive values
    sliderValues$health_value <- 1 + input$health / 100
    sliderValues$house_value <- 1 + input$house / 100
    sliderValues$jobs_value <- 1 + input$jobs / 100
    sliderValues$edu_value <- 1 + input$edu / 100
    sliderValues$choice_value <- input$choice
    
    # Update the selected commune when the button is pressed
    selected_commune(input$choice)
  })
  
  output$selected_option <- renderText({
    paste("You selected:", selected_commune())  # Use the updated commune variable
  })
  
  # Reactive expression to create a data frame of all input values
  sliderData <- reactive({
    data.frame(
      Name = c("commune","Health services:", "House/Rental Prices:", "Job Opportunities:", "Education:"),
      Value = c(sliderValues$choice_value,sliderValues$health_value, sliderValues$house_value, sliderValues$jobs_value, sliderValues$edu_value),
      stringsAsFactors = FALSE
    )
  })
  
  output$values <- renderTable({
    sliderData()
  })
  
  output$map <- renderPlot({
    visual_option <- sliderData()[1, 2]
    slider_df <- isolate(sliderData())
    numeric_visual_option <- as.numeric(visual_option)
    options(
      shiny.reactlog = TRUE,
      shiny.reactlog_interval = 1000
    )
    
    # Visualize all the maps
    #baden_commune_map(commune_general_info, commune_general_info$Gesamtbevölkerung, "Population")
    map_plot <- simulate(slider_df,pop_df,"Population",30)  # slider inputs, population dataframe, legend, arrow person limit
    arrow_data <- emigration(pop_df)
    final_plot <- arrow_plot(map_plot, arrow_data, 30)  # Add arrows to the map
    print(final_plot)
  }, width = 900, height = 600)
}


# Run the app ----
shinyApp(ui = ui, server = server)