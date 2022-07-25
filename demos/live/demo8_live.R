library(tidyverse)
library(shiny)

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        fileInput("upload", "Upload a file", accept = ".csv")
      ),
      mainPanel( 
        tableOutput("widget"),
        tableOutput("data")
      )
    )
  ),
  server = function(input, output, session) {
    output$widget = renderTable({
      input$upload
    })
    
    output$data = renderTable({
      x = read_csv(input$upload$datapath)
      
      validate(
        need(
          all(c("state", "city", "time") %in% names(x)),
          "Bad data frame uploaded, need city, state and time columns."      
        )
      )
      
      x
    })
    
  }
)
