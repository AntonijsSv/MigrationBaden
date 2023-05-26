#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  titlePanel(h1(strong("Migration Simulator"))),
  
  sidebarLayout(position = "right",
    sidebarPanel(h4(strong("Migrationfactors:")),
                 sliderInput(inputId = "slider1",
                             label = "Political Orientation",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput(inputId = "slider2",
                             label = "Shopping centre/grocery store proximity",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput(inputId = "slider3",
                             label = "Entertainment possibilities",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput(inputId = "slider4",
                             label = "Proximity to work",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput(inputId = "slider5",
                             label = "Houspreises/Rental prices",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput(inputId = "slider6",
                             label = "Taxes",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput(inputId = "slider7",
                             label = "Public Transport",
                             min = 1,
                             max = 5,
                             value = 3)
                 ),
    mainPanel(h2("Map"), tags$embed(type = "FirstVisual.R"))
  )
)

# Define server logic ----
server <- function(input, output) {
  output$value <- renderPrint({ input$slider1 })
  output$value <- renderPrint({ input$slider2 })
  output$value <- renderPrint({ input$slider3 })
  output$value <- renderPrint({ input$slider4 })
  output$value <- renderPrint({ input$slider5 })
  output$value <- renderPrint({ input$slider6 })
  output$value <- renderPrint({ input$slider7 })}

# Run the app ----
shinyApp(ui = ui, server = server)