
test_that("conseq()", {
  expect_equal(conseq(c(1, 4, 5, 6)), "c(1, 4:6)")
  expect_equal(conseq(1), "1")
  expect_equal(conseq(c(1, 2, 3, 5, 6, 7, 12, 13, 14)), "c(1:3, 5:7, 12:14)")
  expect_equal(conseq(c(1, 2, 5, 6)), "c(1, 2, 5, 6)")

  expect_equal(conseq(c(1, 4, 5, 6), type = "month"), "Jan, Apr-Jun")
  expect_equal(conseq(1, type = "month"), "Jan")
  expect_equal(conseq(13, type = "month"), NA_character_)
})

test_that("plot_title()", {
  s <- list(years_range = c(1900, 2000), water_year = 1, months = 1:12,
            station_name = "Carnation Creek")

  plot_title(s) %>%
    expect_equal("Carnation Creek (1900-2000)")

  plot_title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (1900-2000)")

  s$water_year <- 10

  plot_title(s) %>%
    expect_equal("Carnation Creek (Water Year 1900-2000)")

  plot_title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (Water Year 1900-2000)")

  s$months <- c(1, 2, 3, 4)

  plot_title(s) %>%
    expect_equal("Carnation Creek (Jan-Apr Water Year 1900-2000)")

  plot_title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (Jan-Apr Water Year 1900-2000)")

  s$water_year <- 1

  plot_title(s) %>%
    expect_equal("Carnation Creek (Jan-Apr 1900-2000)")

  plot_title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (Jan-Apr 1900-2000)")

  s$months <- c(1, 2, 3, 4, 12)

  plot_title(s) %>%
    expect_equal("Carnation Creek (Jan-Apr, Dec 1900-2000)")

  plot_title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (Jan-Apr, Dec 1900-2000)")

  s$water_year <- 10

  plot_title(s) %>%
    expect_equal("Carnation Creek (Jan-Apr, Dec Water Year 1900-2000)")

  plot_title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (Jan-Apr, Dec Water Year 1900-2000)")

})
