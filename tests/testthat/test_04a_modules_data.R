# Data Load -------------------
test_that("Data Load - HYDAT", {
  skip("problems on check")
  local_hydat()
  d <- dummy_data()

  testServer(server_data_load, {
    session$setInputs(
      # Loading
      source = "HYDAT", station_number = "08HB048", station_name = "Test",
      hydat_table_rows_all = 1:1000,
      hydat_bc = TRUE, load = 1, plot_title = TRUE,
      # Settings
      years_range = c(1980, 2010), months = 1:12, roll_days = 1,
      roll_align = "right",
      water_year = 1, basin_area = 10.3, daterange = c("1980-01-01", "2010-01-01"),
      discharge = "Value", complete = FALSE, missing = TRUE, allowed = 100)

    session$setInputs(load = 2) # Prompt past ignoreInit on data_raw()

    expect_error(stations(), NA)
    expect_error(output$hydat_map, NA)
    expect_error(output$hydat_table, NA)
    expect_error(output$info, NA)

    # Check that data can be loaded
    expect_error(d <- data_raw(), NA)
    expect_false(is.null(d$Yield_mm))

    # Check plots and tables
    expect_false(output$code == "")
    expect_error(output$plot, NA)
    expect_error(output$table, NA)

    # Check No basin area
    session$setInputs(basin_area = 0)
    expect_error(d <- data_raw(), NA)
    expect_true(is.null(d$Yield_mm))

  }) %>% suppressWarnings()
})

test_that("Data Load - File", {
  skip("problems on check")
  local_hydat()
  d <- dummy_data()
  f <- list.files(system.file("extdata", package = "fasstrshiny"),
                  "test_data", full.names = TRUE) %>%
    data.frame(name = basename(.), datapath = ., row.names = basename(.))

  testServer(server_data_load, {
    session$setInputs(
      # Loading
      source = "CSV", load = 1, plot_title = TRUE, station_name = "Test",
      file = f["test_data.csv", ],
      col_date = "dt", col_value = "flow", col_symbol = "sym",
      # Settings
      years_range = c(1980, 2010), months = 1:12, roll_days = 1, roll_align = "right",
      water_year = 1, basin_area = 10.3, daterange = c("1980-01-01", "2010-01-01"),
      discharge = "Value", complete = FALSE, missing = TRUE, allowed = 100)

    session$setInputs(load = 2) # Prompt past ignoreInit on data_raw()

    expect_error(output$info, NA)

    # Check that data can be loaded
    expect_error(d <- data_raw(), NA)
    expect_false(is.null(d$Yield_mm))
    expect_false(is.null(d$Symbol))

    # Check plots and tables
    expect_false(output$code == "") # For data loading, no plots
    expect_error(output$plot, NA)  # need data_raw()
    expect_error(output$table, NA) # need data_raw()

    # Check No basin area
    session$setInputs(basin_area = 0)
    expect_error(d <- data_raw(), NA)
    expect_true(is.null(d$Yield_mm))
  }) %>% suppressWarnings()


  # Check other files
  testServer(server_data_load, {
    session$setInputs(
      # Loading
      source = "CSV", load = 1, plot_title = TRUE,
      col_date = "dt", col_value = "flow", col_symbol = "sym",
      # Settings
      years_range = c(1980, 2010), months = 1:12, roll_days = 1, roll_align = "right",
      water_year = 1, basin_area = 10.3, daterange = c("1980-01-01", "2010-01-01"),
      discharge = "Value", complete = FALSE, missing = TRUE, allowed = 100,
      station_name = "test")

    session$setInputs(file = f["test_data_dates.csv",])
    expect_error(eval_check(data_raw_file()), "At least one date")

    session$setInputs(file = f["test_data_dups.csv",])
    expect_error(output$data_preview)

    session$setInputs(file = f["test_data_dups_dates.csv",])
    expect_error(eval_check(data_raw_file()), "At least one date")

    session$setInputs(file = f["test_data_sym.csv",])
    expect_error(data_raw())
    session$setInputs(col_symbol = " ")
    expect_error(d <- eval_check(data_raw_file()), NA)
    expect_true(is.null(d$Symbol))

  }) %>% suppressWarnings()
})


# Data Available -----------------
test_that("Data Available", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

    testServer(server_data_available, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(availability = TRUE, stats = "Mean",
                        symbols_sum_type = "dayofyear",
                        available_type = "tile",
                        plot_log = FALSE, months_inc = 1:12,
                        plot_title_summary = TRUE,
                        plot_title_symbols_flow = TRUE,
                        plot_title_symbols_sum = TRUE,
                        plot_title_available = TRUE)

      expect_error(output$plot_summary, NA)
      expect_error(output$plot_symbols_flow, NA)
      expect_error(output$plot_symbols_sum, NA)
      expect_error(output$plot_available, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(symbols_sum_type = "count")
      if(!f) expect_error(output$plot_symbols_sum, NA)
      if(f) expect_error(output$plot_symbols_sum)  ## FOR NOW UNTIL FIXED in fasstr
      expect_false(output$code == "")

      session$setInputs(symbols_sum_type = "percent")
      if(!f) expect_error(output$plot_symbols_sum, NA)
      if(f) expect_error(output$plot_symbols_sum)  ## FOR NOW UNTIL FIXED in fasstr
      expect_false(output$code == "")

      session$setInputs(available_type = "bar")
      expect_error(output$plot_available, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings() %>% suppressMessages()
  }
})


test_that("Data Available - Symbols", {
  local_hydat()
  d <- dummy_data(symbols = FALSE)

  testServer(server_data_available, args = list(d$s, d$d, d$l, d$c), {
    session$setInputs(availability = TRUE, stats = "Mean",
                      symbols_sum_type = "dayofyear",
                      available_type = "tile",
                      plot_log = FALSE, months_inc = 1:12,
                      plot_title_summary = TRUE,
                      plot_title_symbols_flow = TRUE,
                      plot_title_symbols_sum = TRUE,
                      plot_title_available = TRUE)

    expect_error(output$plot_summary, NA)
    expect_error(output$plot_symbols_flow, "Cannot plot")
    expect_error(output$plot_symbols_sum, "Cannot plot")
    expect_error(output$plot_available, NA)
    expect_error(output$table, NA)
    expect_false(output$code == "")
  }) %>% suppressWarnings() %>% suppressMessages()
})
