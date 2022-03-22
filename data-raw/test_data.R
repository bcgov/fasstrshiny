
tidyhydat::hy_daily_flows(station_number = "08HB048") %>%
  dplyr::select(dt = Date, flow = Value, sym = Symbol) %>%
  write.csv("inst/extdata/test_data.csv", row.names = FALSE)
