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
      
    })
  }
)
