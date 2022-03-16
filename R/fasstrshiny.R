# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


#' Launch fasstr shiny app
#'
#' @export
#'
fasstr_shiny <- function() {

  # Setup -------------------------------------
  options(
    spinner.color = "#003366", spinner.type = 5, spinner.size = 0.5, # Spinners
    scipen=999)                                              # No sci notations

  plot_height <- "500px"

  # On shinyapps ----------------------------------
  on_shinyapps <- !identical(serverInfo(), list(shinyServer = FALSE))
  msg <- on_shinyapps

  if(on_shinyapps) {

    # Don't move HYDAT but create a symlink so we can use it as if it was where
    # tidyhydat wanted it to be
    h <- fasstrshiny:::find_hydat()   # Where Hydat is
    th <- hy_dir()      # Where tidyhydat wants it to be

    cat(file = stderr(), "Where hydat is: ", h, "\n")
    cat(file = stderr(), "Where should be: ", th, "\n")
    if(!dir.exists(th)) dir.create(th, recursive = TRUE)
    unlink(file.path(th, basename(h))) # Delete any existing links
    file.symlink(h, th)                # Create a new link

    cat(file = stderr(), "Link: ", list.files(th, full.names = TRUE)[1], " \n")
  }

  if(msg)  cat(file = stderr(), "Getting tidyhydat stations \n")

  # Prep -------------------------------------------
  stations <- prep_hydat()
  bc_hydrozones <- bcmaps::hydrozones(ask = FALSE) %>%
    sf::st_transform(crs = 4326)


  # UI --------------------------------
  ui <- tagList(
    dashboardPage(
      dashboardHeader(title = "fasstr Shiny"),

      ## Sidebar ----------
      dashboardSidebar(
        tags$script(src = "tips.js"),
        sidebar_order(),
        div(style = "margin-top: 10px", gt::gt_output("data-info"))
      ),
      ## Body -----------------
      dashboardBody(
        useShinyjs(),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bcgov.css")),
        tabItems(
          tabItem("home", ui_home()),
          tabItem("data_load", ui_data_load("data", plot_height)),
          tabItem("data_available", ui_data_available("available", plot_height)),
          tabItem("overview", ui_overview("overview", plot_height)),
          tabItem("hydro", ui_hydro("hydro", plot_height)),
          tabItem("cumulative", ui_cumulative("cum", plot_height = plot_height))#,
         # tabItem("flows", ui_flows("flows", plot_height = plot_height)),
        ##  tabItem("as_stats", ui_annual_stats("annual_stats", plot_height = plot_height)),
        ##  tabItem("as_means", ui_annual_means("annual_means", plot_height = plot_height)),
          #tabItem("as_totals", ui_annual_totals("annual_totals", plot_height = plot_height)),
          # tabItem("as_flow_timing", ui_flow_timing("flow_timing", plot_height = plot_height)),
          # tabItem("as_low_flows", ui_low_flows("low_flows", plot_height = plot_height)),
          # tabItem("as_peak_flows", ui_peak_flows("peak_flows", plot_height = plot_height)),
          # tabItem("as_outside_normal", ui_outside_normal("outside_normal", plot_height = plot_height)),
          # tabItem("analysis_annual", ui_annual_trends("annual_trends", plot_height = plot_height)),
          # tabItem("analysis_volume_freq", ui_volume_freq("volume_freq", plot_height = plot_height)),
          # tabItem("analysis_hydat_peak", ui_hydat_peak("hydat_peak", plot_height = plot_height))
        )
      )
    ),

    ## Footer --------------------
    tags$footer(
      div(
        a(href="https://www2.gov.bc.ca/gov/content/home", "Home"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility"),
        " | ",
        a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright"),
        " | ",
        a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact"), class = "bcgov-footer")
    )
  )

  server <- function(input, output, session) {
    # Load data and get settings
    data_outputs <- server_data_load(id = "data", stations = stations,
                                     bc_hydrozones = bc_hydrozones)
    data_settings <- data_outputs$data_settings
    data_raw <- data_outputs$data_raw
    data_loaded <- data_outputs$data_loaded

    # Other modules
    server_data_available(id = "available", data_settings, data_raw, data_loaded)
    server_hydro(id = "hydro", data_settings, data_raw, data_loaded)
    server_cumulative(id = "cum", data_settings, data_raw, data_loaded)
  }

 shinyApp(ui = ui, server = server)
}


sidebar_order <- function() {
  sidebarMenu(
    id = "menu",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("table"),
             menuSubItem("Loading", tabName = "data_load"),
             menuSubItem("Availability", tabName = "data_available")),
    menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
    menuItem("Hydrographs", icon = icon("chart-area"),
             menuSubItem("Daily and Long-term", tabName = "hydro"),
             menuSubItem("Cumulative", tabName = "cumulative")),
    menuItem("Flow duration and percentiles", tabName = "flows",
             icon = icon("clock")),
    menuItem("Annual Statistics", tabName = "annual",
             icon = icon("calendar"),
             menuSubItem("Statistics", tabName = "as_stats"),
             menuSubItem("Means", tabName = "as_means"),
             menuSubItem("Totals", tabName = "as_totals"),
             menuSubItem("Flow timing", tabName = "as_flow_timing"),
             menuSubItem("Low Flows", tabName = "as_low_flows"),
             menuSubItem("Peak Flows", tabName = "as_peak_flows"),
             menuSubItem("Days outside normal", tabName = "as_outside_normal")),
    menuItem("Analyses", tabName = "analyses",
             icon = icon("chart-line"),
             menuSubItem("Annual Trends", tabName = "analysis_annual"),
             menuSubItem("Volume Frequency", tabName = "analysis_volume_freq"),
             menuSubItem("HYDAT Peak", tabName = "analysis_hydat_peak"))
  )
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
      dashboardSidebar(disable = TRUE),
      ## Body -----------------
      dashboardBody(
        useShinyjs(),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bcgov.css")),
        get(paste0("ui_", mod))(mod, plot_height)
      )
    )
  )

  shinyApp(ui = ui, server = server)

}
