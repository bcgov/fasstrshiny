# library(shinytest)
#
# test_that("Module snapshots work", {
#   skip_on_cran()
#
#   # Use compareImages=FALSE because the expected image screenshots were created
#   # on a Mac, and they will differ from screenshots taken on the CI platform,
#   # which runs on Linux.
#
#   # viewTestDiff("tests/testthat/apps/annual_means", "basic")
#
#   expect_pass(testApp(test_path("apps/annual_means"), compareImages = FALSE))
#   #expect_pass(testApp(test_path("apps/annual_stats"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/annual_totals"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/annual_trends"), compareImages = FALSE))
#   #expect_pass(testApp(test_path("apps/cumulative"), compareImages = FALSE))
#   #expect_pass(testApp(test_path("apps/data_load"), compareImages = FALSE))
#   #expect_pass(testApp(test_path("apps/data_available"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/flow_timing"), compareImages = FALSE))
#   #expect_pass(testApp(test_path("apps/flows"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/hydat_peak"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/hydro"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/low_Flows"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/outside_normal"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/high_flows"), compareImages = FALSE))
#   expect_pass(testApp(test_path("apps/volume_freq"), compareImages = FALSE))
# })
