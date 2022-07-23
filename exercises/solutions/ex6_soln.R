library(tidyverse)
library(shiny)
library(shinydashboard)
d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d %>%
  select(where(is.numeric)) %>%
  names() %>%
  {setNames(
    .,
    str_replace_all(., "([A-Z])", " \\1") %>% str_to_title()
  )}

shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title ="Weather Forecasts"
    ),
    dashboardSidebar(
      selectInput(
        "state", "Select a state",
        choices = sort(unique(d$state))
      ),
      selectInput(
        "city", "Select a city",
        choices = c(),
        multiple = TRUE
      )

    ),
    dashboardBody( 
      fluidRow(
        box(
          title = "Forecast", width = 12,
          solidHeader = TRUE, status = "primary",
          selectInput(
            "var", "Select a variable",
            choices = d_vars, selected = "temperature"
          ),
          plotOutput("plot")
        )
      ),
      fluidRow(
        box(
          title = textOutput("minmax_title"),
          solidHeader = TRUE, status = "info",
          tableOutput("minmax")
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    output$minmax_title = renderText({
      paste0("Daily min/max ", names(d_vars)[d_vars == input$var])
    })
    
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
        ggtitle(names(d_vars)[d_vars == input$var]) +
        geom_line() +
        geom_point() +
        theme_minimal()
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
