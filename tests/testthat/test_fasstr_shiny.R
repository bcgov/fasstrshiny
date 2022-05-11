
test_that("fasstr_shiny() components", {

  # Side bar
  expect_silent(s <- sidebar_fasstr()) %>%
    expect_s3_class("shiny.tag")

  # All mod ids appear in the sidebar menu
  purrr::map_lgl(mods, ~stringr::str_detect(as.character(s), .)) %>%
    all() %>%
    expect_true()

  # Footer
  expect_silent(s <- footer_fasstr()) %>%
    expect_s3_class("shiny.tag")
})

test_that("fasstr_shiny()", {
  local_hydat()

  expect_silent(f <- fasstr_shiny()) %>%
    expect_s3_class("shiny.appobj")

  i <- list(source = "HYDAT", station_number = "08HB048",
            hydat_table_rows_all = 1:1000, hydat_bc = TRUE,
            point_colour = "STATUS",
            load = 1, years_range = c(1980, 2010),
            water_year = 1, basin_area = 10.3,
            daterange = c("1980-01-01", "2010-01-01"),
            file = data.frame(
              name = "test_data.csv",
              datapath = system.file("extdata", "test_data.csv", package = "fasstrshiny")),
            col_date = "dt", col_value = "flow", col_symbol = "sym",
            plot_title = TRUE) %>%
    setNames(paste0("data-", names(.)))

  testServer(server_fasstr, {
    do.call(session$setInputs, i)
    expect_error(output$`data-hydat_map`, NA)
    expect_error(output$`data-hydat_table`, NA)

    # helper_shiny.R functions
    expect_silent(fasstr_url_modal("https://bcgov.ca"))
    expect_equal(get_inputs(input, "data-source"), list(`data-source` = "HYDAT"))
  }) %>% suppressWarnings()


})
