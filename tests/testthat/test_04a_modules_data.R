# Data Load -------------------
test_that("Data Load", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)
    if(f) s <- "CSV" else s <- "HYDAT"

    testServer(server_data_load, {
      session$setInputs(
        source = s, station_number = "08HB048",
        hydat_table_rows_all = 1:1000,
        hydat_bc = TRUE, load = 1, years_range = c(1980, 2010),
        water_year = 1, basin_area = 10.3,
        daterange = c("1980-01-01", "2010-01-01"),
        file = data.frame(
          name = "test_data.csv",
          datapath = system.file("extdata", "test_data.csv", package = "fasstrshiny")),
        col_date = "dt", col_value = "flow", col_symbol = "sym",
        plot_title = TRUE)

      expect_error(stations(), NA)
      expect_error(output$hydat_map, NA)
      expect_error(output$hydat_table, NA)
      expect_error(output$info, NA)

      # Check that data can be loaded
      expect_error(data_raw_hydat(), NA)
      expect_error(data_raw_file(), NA)
      expect_silent(eval_check(data_raw_hydat()))
      expect_silent(eval_check(data_raw_file()))

      #expect_error(data_raw(), NA)   # Can't test because of ignoreInit = TRUE
      #expect_error(output$plot, NA)  # need data_raw()
      #expect_error(output$table, NA) # need data_raw()
      #expect_false(output$code == "") # need data_raw()

    }) %>% suppressWarnings()
  }
})

# Data Available -----------------
test_that("Data Available", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

    testServer(server_data_available, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(availability = TRUE, stats = "Mean",
                        symbols_agg_type = "dayofyear",
                        available_type = "tile",
                        plot_log = FALSE, months_inc = 1:12,
                        plot_title_summary = TRUE,
                        plot_title_symbols_flow = TRUE,
                        plot_title_symbols_agg = TRUE,
                        plot_title_available = TRUE)

      expect_error(output$plot_summary, NA)
      expect_error(output$plot_symbols_flow, NA)
      expect_error(output$plot_symbols_agg, NA)
      expect_error(output$plot_available, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(symbols_agg_type = "count")
      if(!f) expect_error(output$plot_symbols_agg, NA)
      if(f) expect_error(output$plot_symbols_agg)
      expect_false(output$code == "")

      session$setInputs(symbols_agg_type = "percent")
      if(!f) expect_error(output$plot_symbols_agg, NA)
      if(f) expect_error(output$plot_symbols_agg)
      expect_false(output$code == "")

      session$setInputs(available_type = "bar")
      expect_error(output$plot_available, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings() %>% suppressMessages()
  }
})
