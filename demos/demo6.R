library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d %>%
  select(where(is.numeric)) %>%
  names()

shinyApp(
  ui = fluidPage(
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state", "Select a state",
          choices = sort(unique(d$state))
        ),
        selectInput(
          "city", "Select a city",
          choices = c(),
          multiple = TRUE
        ),
        selectInput(
          "var", "Select a variable",
          choices = d_vars, selected = "temperature"
        ),
        downloadButton("download")
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output, session) {
    
    output$download = downloadHandler(
      filename = function() {
        paste0(
          paste(input$city,collapse="_"), 
          ".csv"
        )
      },
      content = function(file) {
        readr::write_csv(d_city(), file)
      }
    )
    
    d_city = reactive({
      req(input$city)
      d %>%
        filter(city %in% input$city)
    })
    
    observe({
      cities = d %>%
        filter(state %in% input$state) %>%
        pull(city) %>%
        unique() %>%
        sort()
      
      updateSelectInput(
        inputId = "city", 
        choices = cities
      )
    })
    
    output$plot = renderPlot({
      d_city() %>%
        ggplot(aes(x=time, y=.data[[input$var]], color=city)) +
        ggtitle(input$var) +
        geom_line()
    })
    
    output$minmax = renderTable({
      d_city() %>%
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
