library(tidyverse)
library(shiny)
library(shinydashboard)
d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d %>%
  select(where(is.numeric)) %>%
  names()

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Weather Forecasts"),
    dashboardSidebar(
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
      )
    ),
    dashboardBody(
      fluidRow(
        box(
          width=7,  title = "Forecast",
          solidHeader = TRUE, status = "success",
          plotOutput("plot")
        ),
        column(
          width=5,
          fluidRow(
            box(
              title = textOutput("table_title"),
              solidHeader = TRUE, status = "danger",
              tableOutput("minmax"), width=12
            )
          ),
          fluidRow(
            infoBox("Hello world", 100, "Subtitle", icon=icon("download"), width=6),
            valueBox(50, "subtitle", icon=icon("github"), color="blue", width=6)
          )
        )

      )
    )
  ),
  server = function(input, output, session) {
    
    d_city = reactive({
      req(input$city)
      d %>%
        filter(city %in% input$city)
    })
    
    output$table_title = renderText({
      paste("Min/Max of ", input$var)
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
