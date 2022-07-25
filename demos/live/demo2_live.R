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
          choices = c("Washington", "New York", "Los Angeles", "Chicago")
        ),
        hr(),
        h4("Options"),
        checkboxInput(
          "forecast", "Highlight forecasted data", value = FALSE
        )
      ),
      mainPanel( plotOutput("plot") )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      g = d %>%
        filter(city %in% input$city) %>%
        ggplot(aes(x=time, y=temperature, color=city)) +
        geom_line()
      
      if (input$forecast) {
        g = g + geom_rect(
          data = d %>%
            filter(forecast) %>%
            summarize(xmin = min(time)),
          aes(xmin=xmin),
          ymin = -Inf, ymax = Inf, xmax = Inf,
          alpha = 0.25, color = NA, fill = "yellow",
          inherit.aes = FALSE
        )
      }
      
      g
    })
  }
)
