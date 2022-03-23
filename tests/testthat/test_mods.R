
# Basic Modules ----------------
test_that("Basic modules", {

  ## Setup -------
  d <- dummy_data()

  ## Annual means ---------------
  testServer(server_annual_means, args = list(d$s, d$d, d$l), {
    expect_silent(output$plot)
    expect_silent(output$code)
  }) %>% suppressWarnings()

  ## Annual stats ----------------
  testServer(server_annual_stats, args = list(d$s, d$d, d$l), {
    session$setInputs(plot_log = TRUE, type = "Monthly", months_plot = 1:12,
                      percentiles = c(10, 90))
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(type = "Annual")
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

  ## Annual totals ----------------

  testServer(server_annual_totals, args = list(d$s, d$d, d$l), {

    session$setInputs(discharge2 = TRUE, display = "Total_Yield")
    expect_error(plots(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(display = "Two_Seasons_Yield")
    expect_error(plots(), NA)
    expect_error(output$plot, NA)

    session$setInputs(display = "Four_Seasons_Yield")
    expect_error(plots(), NA)
    expect_error(output$plot, NA)

    session$setInputs(discharge2 = FALSE, display = "Total_Volume")
    expect_error(plots(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(display = "Two_Seasons_Volume")
    expect_error(plots(), NA)
    expect_error(output$plot, NA)

    session$setInputs(display = "Four_Seasons_Volume")
    expect_error(plots(), NA)
    expect_error(output$plot, NA)

  }) %>% suppressWarnings()


  ## Flow timing ------------------
  testServer(server_flow_timing, args = list(d$s, d$d, d$l), {
    session$setInputs(percent = c(25, 33, 50, 75))

    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

  ## Low flows ------------------
  testServer(server_low_flows, args = list(d$s, d$d, d$l), {
    session$setInputs(roll_days = c(1,3, 7, 30), roll_align = "right",
                      display = "Annual_Low_Flows")

    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(display = "Annual_Low_Flows_Dates")
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

  ## Outside normal --------------------
  testServer(server_outside_normal, args = list(d$s, d$d, d$l), {
    session$setInputs(normal = c(25, 75))

    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()


  ## Peak flows --------------------
  testServer(server_peak_flows, args = list(d$s, d$d, d$l), {
    session$setInputs(roll_day = 1, roll_align = "right")

    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

})

# Data Modules -------------------------
test_that("Data Modules", {

  ## Setup -------
  d <- dummy_data()

  ## Data Load -------------------
  testServer(server_data_load, args = list(bc_hydrozones), {
    session$setInputs(
      source = "HYDAT", station_num = "08HB048",
      hydat_bc = TRUE, load = 1, years_range = c(1980, 2010),
      water_year = 1, basin_area = 10.3,
      daterange = c("1980-01-01", "2010-01-01"),
      file = data.frame(
        name = "test_data.csv",
        datapath = system.file("extdata", "test_data.csv", package = "fasstrshiny")),
      col_date = "dt", col_value = "flow", col_symbol = "sym")

    expect_error(stations(), NA)
    expect_error(output$hydat_map, NA)
    expect_error(output$hydat_table, NA)
    #expect_error(data_raw(), NA)   # Can't test because of ignoreInit = TRUE
    #expect_error(output$plot, NA)  # need data_raw()
    #expect_error(output$table, NA) # need data_raw()
    expect_error(output$info, NA)
    expect_error(output$code, NA)
  }) %>% suppressWarnings()

  ## Data Available -----------------
  testServer(server_data_available, args = list(d$s, d$d, d$l), {
    session$setInputs(availability = TRUE, stats = "Mean", symbols_type = "Days",
                      symbols_percent = FALSE, available_type = "tile",
                      plot_log = FALSE, months_inc = 1:12)
    expect_error(output$plot_summary, NA)
    expect_error(output$plot_symbols, NA)
    expect_error(output$plot_available, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(symbols_type = "Flow")
    expect_error(output$plot_symbols, NA)

  }) %>% suppressWarnings()

})

# Flows and Hydrograph Modules ------------------------
test_that("Flows and Hydrograph Modules don't have errors", {

  ## Setup -------
  d <- dummy_data()

  ## Flows -----------------
  testServer(server_flows, args = list(d$s, d$d, d$l), {
    session$setInputs(months = 1:12, longterm = TRUE, custom_months = "",
                      custom_months_label = "", plot_log = TRUE, update = 0)

    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

  ## Hydro -------------
  testServer(server_hydro, args = list(d$s, d$d, d$l), {
    session$setInputs(type = "Daily", mad = c(5, 10, 20),
                      inner_percentiles = c(27, 75), outer_percentiles = c(5, 95),
                      extra_percentiles = c(5, 25, 75, 95),
                      plot_log = TRUE, include_extreme = TRUE, add_year = "",
                      add_date = "", add_mad = FALSE,
                      custom_months = "", custom_months_label = "")

    expect_error(mad(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(type = "Long-term Daily")
    expect_error(mad(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(type = "Long-term Monthly")
    expect_error(mad(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

  ## Cumulative -----------------------
  testServer(server_cumulative, args = list(d$s, d$d, d$l), {
    session$setInputs(type = "Daily", discharge2 = TRUE, plot_log = FALSE,
                      add_year = "")
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

    session$setInputs(type = "Monthly")
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()


})

# Analysis Modules --------------------
test_that("Analyses Modules", {

  ## Setup -------
  d <- dummy_data()

  ## Annual Trends -----------------------------
  testServer(server_annual_trends, args = list(d$s, d$d, d$l), {
    session$setInputs(
      compute = 1, zyp = "zhang", alpha = 0.05,
      annual_percentiles = c(10, 90), monthly_percentiles = c(10, 20),
      low_roll_days = c(1, 3, 7, 30), low_roll_align = "right",
      percent = c(25, 33, 50, 75), normal = c(25, 75),
      allowed_annual = 100, allowed_monthly = 100,
      table_fit_rows_selected = 1)
    expect_error(trends(), NA)
    expect_error(stat(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table_fit, NA)
    expect_error(output$table_years, NA)
    expect_error(output$code, NA)
  }) %>% suppressWarnings()

  ## Volume Freq ---------------------------------
  testServer(server_volume_freq, args = list(d$s, d$d, d$l), {
    session$setInputs(compute = 1, use_max = FALSE, use_log = FALSE,
                      roll_day = c(1, 3, 7, 30), roll_align = "right",
                      missing = 100, prob_plot = "weibull",
                      prob_scale = "0.999, 0.99, 0.9, 0.5, 0.2, 0.1, 0.01",
                      plot_curve = TRUE,
                      fit_quantiles = c(0.975, 0.99, 0.98, 0.95, 0.9, 0.8,
                                        0.5, 0.2, 0.1, 0.05, 0.01),
                      fit_distr = "PIII", fit_distr_method = "MOM",
                      day = "1-Day")

    expect_error(freqs(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table_plot, NA)
    expect_error(output$table_fit, NA)
    expect_error(output$fit_stats, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings() %>% suppressMessages()


  ## Hydat Peak -----------------------------
  testServer(server_hydat_peak, args = list(d$s, d$d, d$l), {
    session$setInputs(compute = 1, use_max = FALSE, use_log = FALSE,
                      prob_plot = "weibull",
                      prob_scale = "0.999, 0.99, 0.9, 0.5, 0.2, 0.1, 0.01",
                      plot_curve = TRUE,
                      fit_quantiles = c(0.975, 0.99, 0.98, 0.95, 0.9, 0.8,
                                        0.5, 0.2, 0.1, 0.05, 0.01),
                      fit_distr = "PIII", fit_distr_method = "MOM")

    expect_error(freqs(), NA)
    expect_error(output$plot, NA)
    expect_error(output$table, NA)
    expect_error(output$fit_stats, NA)
    expect_error(output$fit_plot, NA)
    expect_error(output$code, NA)

  }) %>% suppressWarnings()

})
