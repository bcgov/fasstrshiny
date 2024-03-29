test_that("code_format()", {
  code <- list("extra" = "AA", "apple" = "XZ",
               "first" = "A", "third" = "B", "second" = "C")
  labels <- c("extra" = "Extra Comment", "apple" = "Apple Comment",
              "first" = "First Comment", "second" = "Second Comment",
              "third" = "Third Comment")
  order <- c("first", "second", "third")

  expect_equal(code_order(names(code), order),
               c("first", "second", "third", "apple", "extra"))

  expect_equal(code_format(code, labels, order = order),
               glue::glue("# First Comment\nA\n\n# Second Comment\nC\n\n",
                          "# Third Comment\nB\n\n# Apple Comment\nXZ\n\n",
                          "# Extra Comment\nAA"))

  expect_equal(code_order(names(code), order = c("lsdjkf", "sldjf")),
               sort(names(code)))

})

test_that("get_css()", {
  expect_type(get_css(), "character")
})

test_that("conseq()", {
  expect_equal(conseq(c(1, 4, 5, 6)), "c(1, 4:6)")
  expect_equal(conseq(1), "1")
  expect_equal(conseq(c(1, 2, 3, 5, 6, 7, 12, 13, 14)), "c(1:3, 5:7, 12:14)")
  expect_equal(conseq(c(1, 2, 5, 6)), "c(1, 2, 5, 6)")

  expect_equal(conseq(c(1, 4, 5, 6), type = "month"), "Jan, Apr-Jun")
  expect_equal(conseq(1, type = "month"), "Jan")
  expect_equal(conseq(13, type = "month"), NA_character_)

  expect_equal(conseq(c(1990, 1991, 1992, 1993)), "c(1990:1993)")

  expect_equal(conseq(c(1990, 1991, 1992, 1993), wrap = FALSE), "1990:1993")
})

test_that("conseq_wy()", {
  expect_equal(conseq_wy(c(1, 4, 5, 6)), "Jan, Apr-Jun")
  expect_equal(conseq_wy(c(1, 4, 5, 6), wy = 2), "Apr-Jun, Jan")
  expect_equal(conseq_wy(c(1, 4, 5, 6), wy = 4), "Apr-Jun, Jan")
  expect_equal(conseq_wy(c(1, 4, 5, 6), wy = 5), "May, Jun, Jan, Apr")
  expect_equal(conseq_wy(c(1, 4, 5, 6), wy = 6), "Jun, Jan, Apr, May")
  expect_equal(conseq_wy(c(1, 4, 5, 6), wy = 7), "Jan, Apr-Jun")
})


test_that("title()", {
  s <- list(years_range = c(1900, 2000), water_year = 1, months = 1:12,
            station_name = "Carnation Creek", station_id = "08HB048",
            source = "HYDAT")

  title(s) %>%
    expect_equal("Carnation Creek - 08HB048 (1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek - 08HB048 (1900-2000)")

  s$water_year <- 10

  title(s) %>%
    expect_equal("Carnation Creek - 08HB048 (Water Year 1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek - 08HB048 (Water Year 1900-2000)")

  s$months <- c(1, 2, 3, 4)

  title(s) %>%
    expect_equal("Carnation Creek - 08HB048 (Jan-Apr Water Year 1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek - 08HB048 (Jan-Apr Water Year 1900-2000)")

  s$water_year <- 1

  title(s) %>%
    expect_equal("Carnation Creek - 08HB048 (Jan-Apr 1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek - 08HB048 (Jan-Apr 1900-2000)")

  s$months <- c(1, 2, 3, 4, 12)

  title(s) %>%
    expect_equal("Carnation Creek - 08HB048 (Jan-Apr, Dec 1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek - 08HB048 (Jan-Apr, Dec 1900-2000)")

  s$water_year <- 10

  title(s) %>%
    expect_equal("Carnation Creek - 08HB048 (Jan-Apr, Dec Water Year 1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek - 08HB048 (Jan-Apr, Dec Water Year 1900-2000)")


  s$source <- "CSV"

  title(s) %>%
    expect_equal("Carnation Creek (Jan-Apr, Dec Water Year 1900-2000)")

  title(s, "Annual Means") %>%
    expect_equal("Annual Means: Carnation Creek (Jan-Apr, Dec Water Year 1900-2000)")

})

