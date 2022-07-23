options(shiny.fullstacktrace=TRUE)

library(tidyverse)
library(shiny)
d_orig = readr::read_csv(here::here("data/weather.csv"))

d_vars = d_orig %>%
  select(where(is.numeric)) %>%
  names() %>%
  {setNames(
    .,
    str_replace_all(., "([A-Z])", " \\1") %>% str_to_title()
  )}

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
            plotOutput("plot"),
            tableOutput("minmax")    
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
    d = reactiveVal(d_orig)
    d_new = reactiveVal(NULL)
    
    d_city = reactive({
      req(input$city)
      d() %>%
        filter(city %in% input$city)
    })
    
    observe({
      data = readr::read_csv(input$upload$datapath)
      
      d_new(data)
      d_new_cols = names(data)
      
      choices = names(d_orig)
      
      select_elems = imap(
        d_new_cols,
        function(x, i) {
          selectInput(
            inputId = paste0("col_sel", i),
            label = paste0("Column matching `", x,"`:"),
            choices = c("", choices), 
            selected = choices[ pmatch(x, choices) ] %>%
              replace_na("")
          )
        }
      )
      
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
      cur_cols = names(d_new())
      
      rename_cols = map_chr(
        seq_along(cur_cols),
        ~ input[[paste0("col_sel", .x)]] 
      )

      select_vals = setNames(
        cur_cols[rename_cols != ""],
        rename_cols[rename_cols != ""]
      )
      
      d_new_renamed = d_new() %>% 
        select(select_vals)
      
      d(
        bind_rows(d(), d_new_renamed)
      )
      
      # Cleanup
      d_new(NULL)
      output$col_match = renderUI({
        list("Successfully added ", nrow(d_new_renamed), " rows  of data!")
      })
    }) %>%
      bindEvent(input$append)
    
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
