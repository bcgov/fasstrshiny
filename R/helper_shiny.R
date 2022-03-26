
# UI Elements -----------------------------------
ui_rcode <- function(id) {
  tabPanel(title = "R Code", verbatimTextOutput(NS(id, "code")))
}

ui_plot_selection <- function(id) {
  conditionalPanel(
    "output.plot", ns = NS(id),
    helpText("Click on a point or 'lasso'", lasso_svg(),
             " a bunch to add year to ",
             "'Years to exclude'. Remember to re-",
             strong("Compute Trends"), "."))

}

# Restoring dynamic inputs on bookmarking --------------------------

#' Restore inputs
#' Note that each input type, must get it's own update functon. Also
#' note that some use `value` and some `selected`.
#' `delay` is for making sure these update *after* the dynamic inputs have
#' evaluated to their default values. Otherwise this restore is overriddent
#' back to default values
#'
#' @noRd
restore_inputs <- function(session, i, values, delay = 1000) {

  pretty_inputs <- c("plot_title", "plot_log", "plot_extremes", "add_mad")
  selectize_inputs <- c("col_date", "col_value", "col_symbol",
                        "add_year", "add_dates", "years_exclude", "months")
  slider_inputs <- c("years_range", "allowed_annual", "allowed_monthly")
  text_inputs <- "station_name"
  numeric_inputs <- "basin_area"
  radiogroup_inputs <- "water_year"

  shinyjs::delay(delay, {
    i[i %in% pretty_inputs] %>%
      purrr::map(~updatePrettySwitch(session, ., value = values[[.]]))

    i[i %in% selectize_inputs] %>%
      purrr::map(~updateSelectizeInput(session, ., selected = values[[.]]))

    i[i %in% slider_inputs] %>%
      purrr::map(~updateSliderInput(session, ., value = values[[.]]))

    i[i %in% text_inputs] %>%
      purrr::map(~updateTextInput(session, ., value = values[[.]]))

    i[i %in% numeric_inputs] %>%
      purrr::map(~updateNumericInput(session, ., value = values[[.]]))

    i[i %in% radiogroup_inputs] %>%
      purrr::map(~updateRadioGroupButtons(session, ., selected = values[[.]]))
  })
}





# Checking inputs for change ------------------------

update_on_change <- function(session, id, btn = "compute",
                             current, last,
                             labels, styles = c("danger", "secondary")) {
  isolate({
    if(!is.logical(all.equal(current, last))) {
      updateButton(
        session, NS(id, btn), style = styles[1],
        label = labels[1])
    } else {
      updateButton(
        session, NS(id, btn), style = styles[2],
        label = labels[2])
    }
  })
}

get_inputs <- function(input, which) {
  purrr::map(which, ~input[[.]]) %>%
    setNames(which)
}


#' Normally when UI elements are hidden, they aren't evaluated.
#' This is a pain when they need to be evaluated to at least their default
#' values. Use this function whenever a UI is dynamically created (renderUI()).
#' Note that the UI must be identified with an id containing "ui_"
#'
#' @noRd
stop_ui_suspend <- function(id, output) {
  names(outputOptions(output)) %>%
    stringr::str_subset(glue::glue("{id}-ui_")) %>%
    stringr::str_extract("ui_(.)+$") %>%
    purrr::map(~outputOptions(output, ., suspendWhenHidden = FALSE))
}



# Data / Code Checks -----------------------------


#' Evaluate and check for errors
#'
#' Takes text function expression, parses and evaluates it then checks for
#' errors. Any errors are passed through to the Shiny app.
#' @noRd
eval_check <- function(t) {
  t <- try(eval(parse(text = t), envir = parent.frame(n = 1)), silent = TRUE)
  if("try-error" %in% class(t)) {
    validate(need(FALSE, attr(t, "condition")$message),
             errorClass = "red") # becomes shiny-output-error-red
  }
  t
}


#' Messages if data not loaded
#' @noRd
check_data <- function(x){
  validate(need(x, "You'll need to first load some data under Data > Loading"))
}

#' For multiple, sequential need() inside a validate
#' <https://shiny.rstudio.com/articles/validation.html#then>
#' @noRd

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}



# Module testing function --------------------------------------------------

test_mod <- function(mod, hydat_stn = "08HB048") {


  server <- function(input, output, session) {

    d <- dummy_data(hydat_stn)

    if(mod == "data_load") {
      server_data_load(id = mod)
    } else if(mod == "home") {
      moduleServer("home", function(input, output, session) {})
      } else {
      get(paste0("server_", mod))(id = mod, d$s, d$d, d$l)
    }
  }

  ui <- tagList(
    dashboardPage(
      dashboardHeader(title = "Test"),
      ## Sidebar ----------
      dashboardSidebar(),
      ## Body -----------------
      dashboardBody(
        shinyjs::useShinyjs(),
        includeCSS(system.file("shiny_app", "www", "bcgov.css", package = "fasstrshiny")),
        get(paste0("ui_", mod))(mod)
      )
    )
  )

  shinyApp(ui = ui, server = server)

}



dummy_data <- function(hydat_stn = "08HB048") {

  data_raw <- fasstr::fill_missing_dates(station_number = hydat_stn) %>%
    fasstr::add_date_variables(water_year_start = 1) %>%
    fasstr::add_daily_volume() %>%
    fasstr::add_daily_yield()

  data_settings <- list(
    discharge = "Value",
    water_year = 1,
    years_range = c(min(data_raw$WaterYear), max(data_raw$WaterYear)),
    years_exclude = NULL,
    months = 1:12,
    roll_days = 1,
    roll_align = "right",
    complete = FALSE,
    missing = TRUE,
    allowed = 100,
    basin_area = 10.3,
    station_name = "Carnation Creek At The Mouth")

  list("d" = reactive(data_raw),
       "s" = reactive(data_settings),
       "l" = reactiveVal(TRUE))
}
