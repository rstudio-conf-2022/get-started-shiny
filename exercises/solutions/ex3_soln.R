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
        checkboxInput("forecast", "Highlight forecasted data", value = FALSE)
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      g = d %>%
        filter(city %in% input$city) %>%
        ggplot(aes(x=time, y=temperature, color=city)) +
        geom_line()
      
      if (input$forecast) {
        g = g + geom_rect(inherit.aes = FALSE,
          data = d %>%
            filter(forecast) %>%
            group_by(forecast) %>%
            summarize(xmin = min(time)),
          aes(xmin=xmin),
          ymin = -Inf, ymax = Inf, xmax=Inf,
          alpha = 0.25, color = NA, fill = "yellow"
        )
      }
      
      g
    })
    
    output$minmax = renderTable({
      d %>%
        filter(city %in% input$city) %>%
        mutate(
          day = lubridate::wday(time, label = TRUE, abbr = FALSE),
          date = as.character(lubridate::date(time))
        ) %>%
        group_by(date, day) %>%
        summarize(
          `min temp` = min(temperature),
          `max temp` = max(temperature),
          .groups = "drop"
        )
    })
  }
)
