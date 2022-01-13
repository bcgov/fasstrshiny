library(fasstr)
library(tidyverse)
library(tidyhydat)

d <- fill_missing_dates(station_number = "08HB048") %>%
  add_date_variables(water_year_start = 1) %>%
  add_daily_volume() %>%
  add_daily_yield()

plot_daily_stats(d, values = Yield_mm)


calc_longterm_mean(d, percent_MAD = c(5, 10 ,50), complete_years = TRUE)
