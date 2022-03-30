# Annual Trends -----------------------------
test_that(" Annual Trends", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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

# Volume Freq ---------------------------------
test_that("Volume Freq ", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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
  local_hydat()

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

  d <- dummy_data(local_file = TRUE)

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
