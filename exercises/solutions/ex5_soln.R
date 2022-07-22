library(tidyverse)
library(shiny)

shinyApp(
  ui = fluidPage(
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        fileInput("upload", "Upload a file", accept = ".csv"),
        selectInput(
          "state", "Select a state",
          choices = c()
        ),
        selectInput(
          "city", "Select a city",
          choices = c(),
          multiple = TRUE
        ),
        selectInput(
          "var", "Select a variable",
          choices = c(), selected = "temperature"
        )
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output) {
    
    d = reactive({
      req(input$upload)
      readr::read_csv(input$upload$datapath)
    })
    
    d_vars = reactive({
      d() %>%
      select(where(is.numeric)) %>%
      names() %>%
      {setNames(
        .,
        str_replace_all(., "([A-Z])", " \\1") %>% str_to_title()
      )}
    })
    
    d_city = reactive({
      req(input$city)
      d() %>%
        filter(city %in% input$city)
    })
    
    observe({
      states = sort(unique(d()$state))
      
      updateSelectInput(
        inputId = "state", 
        choices = states, selected = states[1]
      )
    })
    
    observe({
      updateSelectInput(
        inputId = "var", 
        choices = d_vars(), selected = d_vars()[1]
      )
    })
    
    observe({
      cities = d() %>%
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
        ggtitle(names(d_vars())[d_vars() == input$var]) +
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
