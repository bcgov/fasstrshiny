# Functions ONLY for testing ----------------------------
# Any packages used here (and only here) go in Suggests

# Use tiny hydat testing database
# Adapted from output of withr::local_ but with withr::defer (otherwise had
# namespace error)
local_hydat <- function(new = system.file("extdata", "test_hydat.sqlite3",
                                          package = "fasstrshiny"),
                        .local_envir = parent.frame()) {
  NULL
  old <- tidyhydat::hy_set_default_db(hydat_path = new)
  withr::defer(tidyhydat::hy_set_default_db(old), envir = .local_envir)
  invisible(old)
}
