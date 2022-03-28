
# Use tiny hydat testing database
library(withr)
local_hydat <- local_(tidyhydat::hy_set_default_db)
hy <- system.file("extdata", "test_hydat.sqlite3", package = "fasstrshiny")

test_that("fasstr_shiny()", {

  local_hydat(hy)

  i <- list(source = "HYDAT", station_number = "08HB048",
            hydat_table_rows_all = 1:1000,
            hydat_bc = TRUE, load = 1, years_range = c(1980, 2010),
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

  }) %>% suppressWarnings()


})
