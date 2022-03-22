set.seed(111)

d <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>%
  dplyr::select(dt = Date, flow = Value, sym = Symbol) %>%
  dplyr::slice_sample(n = 100)


# Good data
write.csv(d, "inst/extdata/test_data.csv", row.names = FALSE)

# Duplicated data
dplyr::bind_rows(d, d) %>%
  write.csv("inst/extdata/test_data_dups.csv", row.names = FALSE)

# Bad dates
d %>%
  dplyr::mutate(dt = format(dt, "%m/%d/%y")) %>%
  write.csv("inst/extdata/test_data_dates.csv", row.names = FALSE)


# Bad dates and duplicated
d %>%
  dplyr::mutate(dt = format(dt, "%m/%d/%y")) %>%
  dplyr::bind_rows(., .) %>%
  write.csv("inst/extdata/test_data_dups_dates.csv", row.names = FALSE)

