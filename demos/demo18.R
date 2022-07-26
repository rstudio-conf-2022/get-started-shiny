library(tidyverse)
library(shiny)
d_orig = readr::read_csv(here::here("data/weather.csv"))

d_vars = d_orig %>%
  select(where(is.numeric)) %>%
  names()

shinyApp(
  ui = fluidPage(
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state", "Select a state",
          choices = sort(unique(d_orig$state))
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
        tabsetPanel(
          tabPanel(
            "Weather",
            plotOutput("plot")
          ),
          tabPanel(
            "Upload",
            fileInput("upload", "Upload additional data", accept = ".csv"),
            uiOutput("col_match")
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    d = reactiveVal(d_orig)   # store for modified df 
    d_new = reactiveVal(NULL) # temp store for new df (to be appended)
    
    observe({
      d_new(
        readr::read_csv(input$upload$datapath)
      )
      
      new_cols = names(d_new()) # new column names
      cur_cols = names(d())     # current column names
      
      # Create the dynamic select inputs
      #   selected value based on partial matching
      select_elems = lapply(
        seq_along(new_cols),
        function(i) {
          selectInput(
            inputId = paste0("colsel",i),
            label = paste0("Column matching `", new_cols[i],"`"),
            choices = c("", cur_cols),
            selected = cur_cols[ pmatch(new_cols[i], cur_cols) ] %>%
              replace_na("")
          )
        }
      )
      
      # Show select elements and helper buttons
      output$col_match = renderUI({
        list(
          select_elems,
          actionButton("append", "Append Data", class = "btn-success"),
          actionButton("cancel", "Cancel")
        )
      })
    }) %>%
      bindEvent(input$upload)
    
    observe({
      output$col_match = renderUI({})
      d_new(NULL)
    }) %>%
      bindEvent(input$cancel)
    
    observe({
      new_cols = names(d_new())
      
      choices = map_chr(
        seq_along(new_cols), 
        ~ input[[paste0("colsel", .x)]]
      )
      
      d_new_renamed = d_new() %>%
        setNames(choices) %>%
        {.[,choices != ""]}
      
      d(
        bind_rows( d(), d_new_renamed )
      )
      
      # Cleanup
      d_new(NULL)
      output$col_match = renderUI({
        list("Successfully added ", nrow(d_new_renamed), " rows  of data!")
      })
    }) %>%
      bindEvent(input$append)
    
    # Previous code
    d_city = reactive({
      req(input$city)
      d() %>%
        filter(city %in% input$city)
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
        ggtitle(input$var) +
        geom_line()
    })
  }
)
