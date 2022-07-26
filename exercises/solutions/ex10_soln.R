library(tidyverse)
library(shiny)

d = readr::read_csv(here::here("data/weather.csv"))
d_new = readr::read_csv(here::here("data/sedona.csv"))

shinyApp(
  ui = fluidPage(
    uiOutput("column_match")
  ),
  server = function(input, output, session) {
    output$column_match = renderUI({
      new_cols = names(d_new)
      cur_cols = names(d)
      
      lapply(
        seq_along(new_cols),
        function(i) {
          selectInput(
            inputId = paste0("colsel",i),
            label = paste0("Column matching `", new_cols[i],"`"),
            choices = c("", cur_cols)
          )
        }
      )
    })
  }
)
