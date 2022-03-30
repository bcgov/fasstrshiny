# Flows -----------------
test_that("Flows", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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

# Hydro -------------
test_that("Hydro", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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

      # Add dates
      session$setInputs(type = "Daily", add_date = "1990-01-01")
      expect_error(mad(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      # Add mad
      session$setInputs(type = "Daily", add_mad = TRUE)
      expect_error(mad(), NA)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})



# Cumulative -----------------------
test_that("Cumulative", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

    testServer(server_cumulative, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(type = "Daily", discharge2 = TRUE, plot_log = FALSE,
                        add_year = "", plot_title = TRUE)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(add_year = 1985)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(type = "Monthly", add_year = "")
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

      session$setInputs(add_year = 1985)
      expect_error(output$plot, NA)
      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()

    # Test no basin area

    d <- dummy_data(local_file = f, basin_area = FALSE)

    testServer(server_cumulative, args = list(d$s, d$d, d$l, d$c), {

      session$setInputs(type = "Daily", discharge2 = TRUE, plot_log = FALSE,
                        add_year = "", plot_title = TRUE)
      expect_error(output$plot, "Cannot calculate yield")
      expect_error(output$table, "Cannot calculate yield")
    }) %>% suppressWarnings()
  }
})
