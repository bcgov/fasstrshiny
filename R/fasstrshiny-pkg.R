#' fasstrshiny
#'
#' Package providing Shiny App user interface to the fasstr package.
#'
#' Get started with `fasstr_shiny()`
#'
#' @docType package
#' @name fasstrshiny-package
#' @aliases fasstrshiny fasstrshiny-pkg fasstrshiny-package
#"
#' @import fasstr
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyBS
#'
#' @importFrom rlang .data .env
#' @importFrom dplyr %>%
NULL


.onLoad <- function (libname, pkgname) { # nolint

  # make some names global to avoid CHECK notes
  utils::globalVariables(".")
  utils::globalVariables("where") # May change in future dplyr/tidyselector

  invisible ()
}
