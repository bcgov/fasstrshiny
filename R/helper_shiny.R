
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

test_mod <- function(mod, hydat_stn) {


  server <- function(input, output, session) {
    data <- fasstr::fill_missing_dates(station_number = hydat_stn) %>%
      fasstr::add_date_variables(water_year_start = 1) %>%
      fasstr::add_daily_volume() %>%
      fasstr::add_daily_yield()

    data_settings <- reactiveValues(
      discharge = "Value",
      water_year = 1,
      years_range = c(min(data$WaterYear), max(data$WaterYear)),
      years_exclude = NULL,
      months = 1:12,
      roll_days = 1,
      roll_align = "right",
      complete = FALSE,
      missing = TRUE,
      allowed = 100)

    data_raw <- reactive({return(data)})

    data_loaded <- reactiveVal(TRUE)

    get(paste0("server_", mod))(id = mod, data_settings, data_raw, data_loaded)
  }

  plot_height <- "500px"
  ui <- tagList(
    dashboardPage(
      dashboardHeader(title = "Test"),
      ## Sidebar ----------
      dashboardSidebar(),
      ## Body -----------------
      dashboardBody(
        useShinyjs(),
        includeCSS(system.file("shiny_app", "www", "bcgov.css", package = "fasstrshiny")),
        get(paste0("ui_", mod))(mod, plot_height)
      )
    )
  )

  shinyApp(ui = ui, server = server)

}
