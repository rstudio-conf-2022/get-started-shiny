library(tidyverse)
library(shiny)

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        fileInput("upload", "Upload a file")
      ),
      mainPanel( 
        tableOutput("widget"),
        tableOutput("data")
      )
    )
  ),
  server = function(input, output) {
    output$widget = renderTable({
      input$upload
    })
    
    output$data = renderTable({
      readr::read_csv(input$upload$datapath)
    })
  }
)
