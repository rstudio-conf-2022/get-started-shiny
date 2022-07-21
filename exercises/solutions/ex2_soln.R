library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "city", "Select a city",
          choices = sort(unique(d$city)),
          selected = "Washington",
          multiple = TRUE
        ) 
      ),
      mainPanel( plotOutput("plot") )
    )
  ),
  server = function(input, output) {
    output$plot = renderPlot({
      d %>%
        filter(city %in% input$city) %>%
        ggplot(aes(x=time, y=temperature, color=city)) +
        geom_line()
    })
  }
)
