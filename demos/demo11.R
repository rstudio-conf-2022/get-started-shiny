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

weatherBoxUI = function(id) {
  infoBoxOutput(NS(id, "info"))
}

weatherBoxServer = function(id, data, var, func, text, color, icon, show_time = FALSE) {
  
  moduleServer(
    id,
    function(input, output, session) {
      output$info = renderInfoBox({
        val = round(func(data()[[var]]),2)
        time = if (show_time) {
          data()$time[data()[[var]] == val] %>%
            format(format="%a, %b %d\n%I:%M %p")
        } else {
          ""
        }
        
        infoBox(
          title = text, value = val, subtitle = time,
          color = color, icon = icon
        )
      })
    }
  )
}


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
        weatherBoxUI("min_temp"),
        weatherBoxUI("max_temp"),
        weatherBoxUI("avg_wind")
      )
    )
  ),
  server = function(input, output) {
    
    weatherBoxServer(
      "min_temp", data = d_city, var = "temperature", func = min, 
      text = "Min temp", color = "blue", icon = icon("temperature-low"),
      show_time = TRUE
    )
    
    weatherBoxServer(
      "max_temp", data = d_city , var = "temperature", func = max, 
      text = "Max temp", color = "red", icon = icon("temperature-high"),
      show_time = TRUE
    )
    
    weatherBoxServer(
      "avg_wind", data = d_city, var = "windSpeed", func = mean, 
      text = "Avg wind", color = "green", icon = icon("wind"),
      show_time = FALSE
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
        ggtitle(names(d_vars)[d_vars == input$var]) +
        geom_line() +
        geom_point() +
        theme_minimal()
    })
  }
)
