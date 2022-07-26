  library(tidyverse)
  library(shiny)
  
  d_orig = readr::read_csv(here::here("data/weather.csv"))[1:24,]
  d_new = readr::read_csv(here::here("data/sedona.csv"))
  
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          uiOutput("column_match"),
          actionButton("append", "Append")
        ),
        mainPanel(
          tableOutput("table")
        )
      )
    ),
    server = function(input, output, session) {
      d = reactiveVal(d_orig)
      
      output$table = renderTable({
        d()
      })
      
      observe({
        new_cols = names(d_new)
        
        choices = map_chr(
          seq_along(new_cols),
          ~ input[[paste0("colsel", .x)]]
        )
        
        d_new_renamed = d_new %>%
          setNames(choices) %>%
          {.[,choices != ""]}
        
        d( bind_rows(
          d(), d_new_renamed
        ) )
      }) %>%
        bindEvent(input$append)
      
      output$column_match = renderUI({
        new_cols = names(d_new)
        cur_cols = names(d())
        
        purrr::imap(
          new_cols,
          function(x, i) {
            selectInput(
              inputId = paste0("colsel", i),
              label = paste0("Column matching `", x, "`"),
              choices = c("", cur_cols),
              selected = cur_cols[ pmatch(x, cur_cols) ] %>%
                replace_na("")
            )
          }
        )
      })
    }
  )
