# Testing modules
#
# - A good first pass
# - Will fail on errors (but note that only the SERVER is tested, not the UI)
# - Will fail if you forget to add an input
# - Will fail if the code doesn't have a label: expect_false(output$code == "")
#   - Fix by adding `labels$XXX <- "R code comment"
#
# Will not check if the figures/tables are what they should be

# Basic Modules ----------------

## Run UIs for errors ------------------------
test_that("UIs", {
  local_hydat()
  for(m in c(mods, "home", "overview")) {
    expect_silent(get(glue::glue("ui_{m}"))('testing'))
  }
})


## Annual means ---------------
test_that("Annual means", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

    testServer(server_annual_means, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(plot_title = TRUE)
      expect_error(output$plot, NA)
      expect_false(output$code == "")
    }) %>% suppressWarnings()
  }
})

## Annual stats ----------------
test_that("Annual stats", {
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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
  local_hydat()

  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

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
  local_hydat()
  for(f in c(FALSE, TRUE)) {
    d <- dummy_data(local_file = f)

    testServer(server_peak_flows, args = list(d$s, d$d, d$l, d$c), {
      session$setInputs(roll_day = 1, roll_align = "right")

      expect_error(output$table, NA)
      expect_false(output$code == "")

    }) %>% suppressWarnings()
  }
})
