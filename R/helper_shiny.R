
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


# Checkin inputs for change ------------------------

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

eval_check <- function(t) {
  t <- try(eval(parse(text = t), envir = parent.frame(n = 1)), silent = TRUE)
  if("try-error" %in% class(t)) {
    validate(need(FALSE, attr(t, "condition")$message),
             errorClass = "red") # becomes shiny-output-error-red
  }
  t
}

#' Messages if data not loaded
check_data <- function(x){
  validate(need(x, "You'll need to first load some data under Data > Loading"))
}

#' For multiple, sequential need() inside a validate
#' <https://shiny.rstudio.com/articles/validation.html#then>

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}



# Module testing function --------------------------------------------------

test_mod <- function(mod, hydat_stn = "08HB048") {


  server <- function(input, output, session) {
    data <- fasstr::fill_missing_dates(station_number = hydat_stn) %>%
      fasstr::add_date_variables(water_year_start = 1) %>%
      fasstr::add_daily_volume() %>%
      fasstr::add_daily_yield()

    data_settings <- reactive({
      list(
        discharge = "Value",
        water_year = 1,
        years_range = c(min(data$WaterYear), max(data$WaterYear)),
        years_exclude = NULL,
        months = 1:12,
        roll_days = 1,
        roll_align = "right",
        complete = FALSE,
        missing = TRUE,
        allowed = 100)})

    data_raw <- reactive({return(data)})

    data_loaded <- reactiveVal(TRUE)

    if(mod == "data_load") {
      server_data_load(id = mod, prep_hydat(), bc_hydrozones)
    } else {
      get(paste0("server_", mod))(id = mod, data_settings, data_raw, data_loaded)
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
