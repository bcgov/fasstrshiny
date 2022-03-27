# Testing modules
#
# - A good first pass
# - Will fail on errors (but note that only the SERVER is tested, not the UI)
# - Will fail if you forget to add an input
# - Will fail if the code doesn't have a label: expect_false(output$code == "")
#   - Fix by adding `labels$XXX <- "R code comment"
#
# Will not check if the figures/tables are what they should be
library(withr)
local_hydat <- local_(tidyhydat::hy_set_default_db)
hy <- system.file("extdata", "test_hydat.sqlite3", package = "fasstrshiny")

# Basic Modules ----------------
test_that("UIs", {
  for(m in mods) expect_silent(get(glue::glue("ui_{m}"))('testing'))
})


## Annual means ---------------
test_that("Annual means", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    expect_equal(tidyhydat::hy_default_db(), hy)

    testServer(server_annual_means, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(plot_title = TRUE)
      expect_error(output$plot, NA)
      expect_false(output$code == "")
    }) %>% suppressWarnings()
  }
})

## Annual stats ----------------
test_that("Annual stats", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_annual_stats, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(plot_log = TRUE, type = "Monthly", months_plot = 1:12,
                        inner_percentiles = c(27, 75),
                        outer_percentiles = c(5, 95),
                        extra_percentiles = c(10, 90), plot_title = TRUE)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(type = "Annual")
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")
    }) %>% suppressWarnings()
  }
})

## Annual totals ----------------
test_that("Annual totals", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_annual_totals, args = list(d$s, d$d, d$l, d$c), {

      session$setInputs(discharge2 = TRUE, display = "Total_Yield",
                        plot_title = TRUE)
      expect_error(plots(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(display = "Two_Seasons_Yield")
      expect_error(plots(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(display = "Four_Seasons_Yield")
      expect_error(plots(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(discharge2 = FALSE, display = "Total_Volume")
      expect_error(plots(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(display = "Two_Seasons_Volume")
      expect_error(plots(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(display = "Four_Seasons_Volume")
      expect_error(plots(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})

## Flow timing ------------------
test_that("Flow timing", {
  local_hydat(hy)

  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_flow_timing, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(percent = c(25, 33, 50, 75), plot_title = TRUE)

      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }

})

## Low flows ------------------
test_that("Low flows", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_low_flows, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(roll_days = c(1,3, 7, 30), roll_align = "right",
                        display = "Annual_Low_Flows", plot_title = TRUE)

      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(display = "Annual_Low_Flows_Dates")
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})

## Outside normal --------------------
test_that("Outside normal", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_outside_normal, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(normal_percentiles = c(25, 75), plot_title = TRUE)

      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})

## Peak flows --------------------
test_that("Peak flows", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_peak_flows, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(roll_day = 1, roll_align = "right")

      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})

# Data Modules -------------------------

## Data Load -------------------
test_that("Data Load", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_data_load, {
      session$setInputs(
        source = "HYDAT", station_number = "08HB048",
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
      #expect_error(data_raw(), NA)   # Can't test because of ignoreInit = TRUE
      #expect_error(output$plot, NA)  # need data_raw()
      #expect_error(output$table, NA) # need data_raw()
      #expect_false(output$code == "") # need data_raw()

    }) %>% suppressWarnings()
  }
})

## Data Available -----------------
test_that("Data Available", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

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

# Flows and Hydrograph Modules ------------------------

## Flows -----------------
test_that("Flows", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_flows, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(months = 1:12, longterm = TRUE, custom_months = "",
                        custom_months_label = "", plot_log = TRUE, update = 0,
                        plot_title = TRUE)

      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})

## Hydro -------------
test_that("Hydro", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_hydro, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(type = "Daily", mad = c(5, 10, 20),
                        inner_percentiles = c(27, 75), outer_percentiles = c(5, 95),
                        extra_percentiles = c(5, 25, 75, 95),
                        plot_log = TRUE, include_extreme = TRUE, add_year = "",
                        add_date = "", add_mad = FALSE,
                        custom_months = "", custom_months_label = "",
                        plot_title = TRUE,
                        add_custom = TRUE, custom = 5, custom_label = "Testing")

      expect_error(mad(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(type = "Long-term Daily")
      expect_error(mad(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(type = "Long-term Monthly")
      expect_error(mad(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})

## Cumulative -----------------------
test_that("Cumulative", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_cumulative, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(type = "Daily", discharge2 = TRUE, plot_log = FALSE,
                        add_year = "", plot_title = TRUE)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(type = "Monthly")
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()

  }
})

# Analysis Modules --------------------

## Annual Trends -----------------------------
test_that(" Annual Trends", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_annual_trends, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(
        compute = 1, zyp = "zhang", alpha = 0.05,
        annual_percentiles = c(10, 90), monthly_percentiles = c(10, 20),
        low_roll_days = c(1, 3, 7, 30), low_roll_align = "right",
        timing_percent = c(25, 33, 50, 75), normal_percentiles = c(25, 75),
        allowed_annual = 100, allowed_monthly = 100,
        table_fit_rows_selected = 1)
      expect_error(trends(), NA)
      expect_error(stat(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table_fit, NA)
      expect_error(output$table_years, NA)
      expect_false(output$code == "")
    }) %>% suppressWarnings()
  }
})

## Volume Freq ---------------------------------
test_that("Volume Freq ", {
  local_hydat(hy)
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(file = f)

    testServer(server_volume_freq, args = list(d$s, d$d, d$l, d$c), {
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
      expect_false(output$code == "")

    }) %>% suppressWarnings() %>% suppressMessages()
  }
})

## Hydat Peak -----------------------------
test_that("Hydat Peak", {
  local_hydat(hy)

  d <- dummy_data()

  testServer(server_hydat_peak, args = list(d$s, d$d, d$l, d$c), {
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
    expect_false(output$code == "")

  }) %>% suppressWarnings()

  d <- dummy_data(file = TRUE)

  testServer(server_hydat_peak, args = list(d$s, d$d, d$l, d$c), {
    session$setInputs(compute = 1, use_max = FALSE, use_log = FALSE,
                      prob_plot = "weibull",
                      prob_scale = "0.999, 0.99, 0.9, 0.5, 0.2, 0.1, 0.01",
                      plot_curve = TRUE,
                      fit_quantiles = c(0.975, 0.99, 0.98, 0.95, 0.9, 0.8,
                                        0.5, 0.2, 0.1, 0.05, 0.01),
                      fit_distr = "PIII", fit_distr_method = "MOM")
    expect_error(freqs(), "This analysis is only available for HYDAT data")
  })

})
