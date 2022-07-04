
test_that("combine_parameters()", {

  combine_parameters("months", 1:12) %>%
    expect_s3_class("glue") %>%
    expect_equal("months = c(1:12)")

  combine_parameters("discharge", "Value") %>%
    expect_s3_class("glue") %>%
    expect_equal("values = 'Value'")

  combine_parameters("percentiles", c(10, 50, 90)) %>%
    expect_s3_class("glue") %>%
    expect_equal("percentiles = c(10, 50, 90)")

  combine_parameters("roll_align", "left") %>%
    expect_s3_class("glue") %>%
    expect_equal("roll_align = 'left'")

})
