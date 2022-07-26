#devtools::install_github("hrbrmstr/darksky")

library(tidyverse)

#darksky::darksky_api_key()

cities = readr::read_csv(here::here("data/cities.csv"))

timestamps = (Sys.Date() + (-2:2)) %>%
  as.character() %>%
  paste0("T00:00:00")

get_darksky_data = function(df) {
  df %>%
    mutate(timestamps = list(timestamps)) %>%
    unnest_longer(timestamps) %>%
    {pmap_df(
      .,
      ~ darksky::get_forecast_for(
        latitude = ..3, longitude = ..4, timestamp = ..5,
        exclude = "currently,minutely,daily,alerts,flags"
      )$hourly %>%
        mutate(
          city = ..1, state = ..2
        )
    )} %>%
    relocate(city, state) %>%
    as_tibble() %>%
    mutate(
      forecast = time > Sys.time()
    )
}

d = cities %>%
  slice(1:50) %>%
  get_darksky_data()

readr::write_csv(d, here::here("data/weather.csv"))


sedona = tibble::tribble(
  ~city,    ~state,    ~lat,    ~long,
  "Sedona", "Arizona", 34.8697, -111.7610
) %>%
  get_darksky_data() %>%
  select(
    city, state, time, temp = temperature, precip_prob = precipProbability, wind = windSpeed, forecast
  )

readr::write_csv(sedona, here::here("data/sedona.csv"))

gaylord = tibble::tribble(
  ~city,    ~state,    ~lat,    ~long,
  "Gaylord National", "Maryland", 38.7814, -77.0168
) %>%
  get_darksky_data() %>%
  select(
    city, state, time, temp = temperature, precip_prob = precipProbability, wind = windSpeed, forecast
  )

readr::write_csv(gaylord, here::here("data/gaylord.csv"))
