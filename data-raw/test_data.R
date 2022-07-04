set.seed(111)


# Test HYDAT -------------------------



# Create small testing Hydat database
file.copy("inst/shiny_app/Hydat.sqlite3", "inst/extdata/test_hydat.sqlite3", overwrite = TRUE)
h <- DBI::dbConnect(RSQLite::SQLite(), "inst/extdata/test_hydat.sqlite3")

t <- DBI::dbListTables(h) %>%
  purrr::map(~DBI::dbListFields(h, .)) %>%
  setNames(DBI::dbListTables(h)) %>%
  purrr::map_lgl(., ~any(.x == "STATION_NUMBER")) %>%
  which() %>%
  names()

purrr::walk(t, ~DBI::dbExecute(h, glue::glue("DELETE FROM {.x} WHERE STATION_NUMBER != '08HB048';")))

DBI::dbExecute(h, glue::glue("DELETE FROM AGENCY_LIST WHERE STATION_NAME != '08HB048';"))
DBI::dbExecute(h, "VACUUM")
DBI::dbDisconnect(h)

# Test files -------------------------

d <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>%
  dplyr::select(dt = Date, flow = Value, sym = Symbol)

# Good data
write.csv(d, "inst/extdata/test_data.csv", row.names = FALSE)

# No symbols
d %>%
  dplyr::select(-sym) %>%
  write.csv("inst/extdata/test_data_sym.csv", row.names = FALSE)


# Duplicated data
dplyr::slice_sample(d, n = 100) %>%
  dplyr::bind_rows(., .) %>%
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

