test_that("shiny utils", {

  local_hydat()

  expect_equal(tidyhydat::hy_default_db(),
               system.file("extdata", "test_hydat.sqlite3",
                           package = "fasstrshiny"))

  expect_silent(ui_rcode("test"))
  expect_silent(ui_plot_selection("test"))
  expect_silent(ui_plotly_info())

  expect_silent(eval_check("ggplot2::ggplot()")) %>%
    expect_s3_class("ggplot")

  expect_error(eval_check("ggpl::ggplot()"))

  expect_silent(t <- test_mod("home")) %>%
    expect_s3_class("shiny.appobj")

  expect_silent(dummy_data()) %>%
    expect_type("list")

})
