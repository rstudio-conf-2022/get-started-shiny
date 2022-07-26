library(tidyverse)
library(shiny)

d = readr::read_csv(here::here("data/weather.csv"))
d_new = readr::read_csv(here::here("data/sedona.csv"))

new_cols = names(d_new)
cur_cols = names(d)


shinyApp(
  ui = fluidPage(
    uiOutput("column_match")
  ),
  server = function(input, output, session) {
    output$column_match = renderUI({
      new_cols = names(d_new)
      cur_cols = names(d)
      
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
