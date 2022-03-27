# Creating shinytest tests

library(shinytest)
library(testthat)

# This is infrastructure to use shinytests to actively tests all the modules in
# headless browsers. But this is super fragile and really computationally
# intensive.
#
# I've left this infrastructure here to create and run the tests in case this
# is a future option



# BUILD PACKAGE!!!!!

# Normally, create tests with this:
shinytest::recordTest(app = test_path("apps/annual_means"), seed = 1001)

# But for the same kind of test for each module, easier to create
# programatically. See tests/testthat/test_shiny.R for actually running the tests

for(m in mods) {

  if(!dir.exists(t1 <- test_path("apps", m))) dir.create(t1)
  if(!dir.exists(t2 <- file.path(t1, "tests"))) dir.create(t2)
  if(!dir.exists(t3 <- file.path(t1, "tests", "shinytest"))) dir.create(t3)

  if(!file.exists(f1 <- file.path(t1, "app.R"))) {
    write(paste0("library(fasstrshiny)\n\ntest_mod(\"", m, "\")"), f1)
  }
  if(!file.exists(f2 <- file.path(t1, "tests", "shinytest.R"))) {
    write("library(shinytest)\nshinytest::testApp(\"../\")", f2)
  }
  if(!file.exists(f3 <- file.path(t1, "tests", "shinytest", "basic.R"))){
    write(paste0("app <- ShinyDriver$new(\"../../\", seed = 1001)\n",
                 "app$snapshotInit(\"basic\")\n\n",
                 "app$snapshot()"),
          f3)
  }
}


