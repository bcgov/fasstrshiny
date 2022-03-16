
ui_rcode <- function(id) {
  tabPanel(title = "R Code", verbatimTextOutput(NS(id, "code")))
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
