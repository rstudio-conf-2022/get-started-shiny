library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = c(
            "Baltimore",
            "Washington", 
            "New York", 
            #"Los Angeles", 
            "Chicago"
          )
        ) 
      ),
      mainPanel( plotOutput("plot") )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      d %>%
        filter(city %in% input$city) %>%
        ggplot(aes(x=time, y=temperature, color=city)) +
        geom_line()
    })
  }
)
