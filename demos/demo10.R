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
        infoBoxOutput("min_temp"),
        infoBoxOutput("max_temp"),
        infoBoxOutput("avg_wind")
      )
    )
  ),
  server = function(input, output) {
    
    output$min_temp = renderInfoBox({
      min_temp = min(d_city()$temperature)
      min_temp_time = d_city()$time[d_city()$temperature == min_temp]
      
      infoBox(
        title = "Min temp",
        value = min_temp,
        subtitle = format(min_temp_time, format="%a, %b %d\n%I:%M %p"),
        color = "blue",
        icon = icon("temperature-low")
      )
    })
    
    output$max_temp = renderInfoBox({
      max_temp = max(d_city()$temperature)
      max_temp_time = d_city()$time[d_city()$temperature == max_temp]
      
      infoBox(
        title = "Max temp",
        value = max_temp,
        subtitle = format(max_temp_time, format="%a, %b %d\n%I:%M %p"),
        color = "red",
        icon = icon("temperature-high")
      )
    })
    
    output$avg_wind = renderInfoBox({
      avg_wind = mean(d_city()$windSpeed)
      
      infoBox(
        title = "Avg wind speed",
        value = round(avg_wind,2),
        color = "green",
        icon = icon("wind")
      )
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
  }
)
