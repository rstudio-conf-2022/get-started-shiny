library(tidyverse)
library(shiny)
library(bslib)

d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d %>%
  select(where(is.numeric)) %>%
  names() %>%
  {setNames(
    .,
    str_replace_all(., "([A-Z])", " \\1") %>% str_to_title()
  )}

shinyApp(
  ui = fluidPage(
    theme = bs_theme(),
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
        )
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax"),
        
        actionButton("b1", "primary", class = "btn-primary"),
        actionButton("b2", "secondary", class = "btn-secondary"),
        actionButton("b3", "success", class = "btn-success"),
        actionButton("b4", "info", class = "btn-info"),
        actionButton("b5", "warning", class = "btn-warning"),
        actionButton("b6", "danger", class = "btn-danger")
      )
    )
  ),
  server = function(input, output, session) {
    bs_themer()
    
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
