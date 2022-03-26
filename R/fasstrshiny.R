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

fasstr_shiny <- function() {

  # Setup -------------------------------------
  options(
    spinner.color = "#003366", spinner.type = 5, spinner.size = 0.5, # Spinners
    scipen=999)                                              # No sci notations

  # On shinyapps ----------------------------------
  on_shinyapps <- !identical(serverInfo(), list(shinyServer = FALSE))

  msg <- on_shinyapps

  if(on_shinyapps) {
    tidyhydat::hy_set_default_db(find_hydat()) # Use packaged HYDAT
    enableBookmarking("url")
  } else {
    enableBookmarking("server")
  }

  # UI --------------------------------
  ui <- function(request) {
    tagList(
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
          shinyjs::useShinyjs(),
          includeCSS(system.file("shiny_app", "www", "bcgov.css",
                                 package = "fasstrshiny")),
          tabItems(
            tabItem("home", ui_home()),
            tabItem("data_load", ui_data_load("data")),
            tabItem("data_available", ui_data_available("data_available")),
            tabItem("hydro", ui_hydro("hydro")),
            tabItem("cumulative", ui_cumulative("cumulative")),
            tabItem("flows", ui_flows("flows")),
            tabItem("annual_stats", ui_annual_stats("annual_stats")),
            tabItem("annual_means", ui_annual_means("annual_means")),
            tabItem("annual_totals", ui_annual_totals("annual_totals")),
            tabItem("flow_timing", ui_flow_timing("flow_timing")),
            tabItem("low_flows", ui_low_flows("low_flows")),
            tabItem("peak_flows", ui_peak_flows("peak_flows")),
            tabItem("outside_normal", ui_outside_normal("outside_normal")),
            tabItem("annual_trends", ui_annual_trends("annual_trends")),
            tabItem("volume_freq", ui_volume_freq("volume_freq")),
            tabItem("hydat_peak", ui_hydat_peak("hydat_peak"))
          )
        )
      ),

      ## Footer --------------------
      tags$footer(
        div(
          div(style = "position:absolute; right: 7px; bottom: 7px",
              bookmarkButton(label = "Bookmark")),
          a(href="https://www2.gov.bc.ca/gov/content/home", "Home"),
          " | ",
          a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer",
            "Disclaimer"),
          " | ",
          a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy"),
          " | ",
          a(href="https://www2.gov.bc.ca/gov/content/home/accessibility",
            "Accessibility"),
          " | ",
          a(href="https://www2.gov.bc.ca/gov/content/home/copyright",
            "Copyright"),
          " | ",
          a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html",
            "Contact"), class = "bcgov-footer")
      )
    )
  }

  server <- function(input, output, session) {
    # Load data and get settings
    data_outputs <- server_data_load(id = "data")
    data_settings <- data_outputs$data_settings
    data_raw <- data_outputs$data_raw
    data_loaded <- data_outputs$data_loaded
    data_code <- data_outputs$data_code

    onBookmarked(function(url) {
      if(on_shinyapps) {
        fasstr_url_modal(url)
      } else showModal(urlModal(url, "Bookmark link"))
    })

    # Other modules
    for(m in c("data_available", "hydro", "cumulative", "flows",
               "annual_stats", "annual_means", "annual_totals", "flow_timing",
               "low_flows", "peak_flows", "outside_normal",
               "annual_trends", "volume_freq", "hydat_peak")) {
      get(glue::glue("server_{m}"))(id = m, data_settings, data_raw,
                                    data_loaded, data_code)
    }
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
             menuSubItem("Statistics", tabName = "annual_stats"),
             menuSubItem("Means", tabName = "annual_means"),
             menuSubItem("Totals", tabName = "annual_totals"),
             menuSubItem("Flow timing", tabName = "flow_timing"),
             menuSubItem("Low Flows", tabName = "low_flows"),
             menuSubItem("Peak Flows", tabName = "peak_flows"),
             menuSubItem("Days outside normal", tabName = "outside_normal")),
    menuItem("Analyses", tabName = "analyses",
             icon = icon("chart-line"),
             menuSubItem("Annual Trends", tabName = "annual_trends"),
             menuSubItem("Volume Frequency", tabName = "volume_freq"),
             menuSubItem("HYDAT Peak", tabName = "hydat_peak"))
  )
}
