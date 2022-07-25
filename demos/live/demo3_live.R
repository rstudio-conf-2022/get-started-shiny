library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d %>%
  select(where(is.numeric)) %>%
  names()
  
shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = c("Washington", "New York", "Los Angeles", "Chicago")
        ),
        selectInput(
          "var", "Select a variable",
          choices = d_vars, selected = "temperature"
        )
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      d %>%
        filter(city %in% input$city) %>%
        ggplot(aes(x=time, y=.data[[input$var]], color=city)) +
        geom_line()
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
          `min` = min(.data[[input$var]]),
          `max` = max(.data[[input$var]]),
          .groups = "drop"
        )
    })
    
  }
)
