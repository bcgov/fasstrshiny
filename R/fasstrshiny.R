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
    scipen=999#,                                              # No sci notations
   # shiny.launch.browser = .rs.invokeShinyWindowExternal
    )

  # On shinyapps ----------------------------------
  on_shinyapps <- !identical(serverInfo(), list(shinyServer = FALSE))

  msg <- on_shinyapps

  if(on_shinyapps) {
    tidyhydat::hy_set_default_db(find_hydat()) # Use packaged HYDAT
    enableBookmarking("url")
  } else {
    enableBookmarking("server")
  }

  css <- get_css()

  # UI --------------------------------
  ui <- function(request) {
    tagList(
      dashboardPage(
        dashboardHeader(title = "fasstrshiny"),

        ## Sidebar ----------
        dashboardSidebar(
          tags$script(src = "tips.js"),
          sidebar_fasstr(),
          div(style = "margin-top: 10px", gt::gt_output("data-info"))
        ),
        ## Body -----------------
        dashboardBody(
          shinyjs::useShinyjs(),
          if(css != "") includeCSS(css),
          tabItems(
            tabItem("home", ui_home()),
            tabItem("data_load", ui_data_load("data")),
            tabItem("data_available", ui_data_available("data_available")),
            tabItem("overview", ui_overview("overview")),
            tabItem("hydro", ui_hydro("hydro")),
            tabItem("cumulative", ui_cumulative("cumulative")),
            tabItem("flows", ui_flows("flows")),
            tabItem("monthly_means", ui_monthly_means("monthly_means")),
            tabItem("annual_stats", ui_annual_stats("annual_stats")),
            tabItem("annual_means", ui_annual_means("annual_means")),
            tabItem("annual_totals", ui_annual_totals("annual_totals")),
            tabItem("flow_timing", ui_flow_timing("flow_timing")),
            tabItem("low_flows", ui_low_flows("low_flows")),
            tabItem("high_flows", ui_high_flows("high_flows")),
            tabItem("annual_extremes", ui_annual_extremes("annual_extremes")),
            tabItem("normal_days", ui_normal_days("normal_days")),
            tabItem("annual_trends", ui_annual_trends("annual_trends")),
            tabItem("volume_freq", ui_volume_freq("volume_freq")),
            tabItem("hydat_peak", ui_hydat_peak("hydat_peak"))
          )
        )
      ),

      ## Footer --------------------
      footer_fasstr()
    )
  }



  shinyApp(ui = ui, server = server_fasstr)
}


server_fasstr <- function(input, output, session) {

  on_shinyapps <- !identical(serverInfo(), list(shinyServer = FALSE))

  # Load data
  data_outputs <- server_data_load(id = "data")

  # Get settings
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
  for(m in mods[mods != "data_load"]) {
    get(glue::glue("server_{m}"))(id = m, data_settings, data_raw,
                                  data_loaded, data_code)
  }
}

sidebar_fasstr <- function() {
  sidebarMenu(
    id = "menu",
    menuItem("Home", tabName = "home", icon = icon("house")),
    menuItem("Data", tabName = "data", icon = icon("table"),
             menuSubItem("Loading & Options", tabName = "data_load",
                         icon = icon("angles-right")),
             menuSubItem("Availability & Screening", tabName = "data_available",
                         icon = icon("angles-right"))),
    menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
    menuItem("Hydrographs", icon = icon("chart-area"),
             menuSubItem("Daily and Long-term", tabName = "hydro",
                         icon = icon("angles-right")),
             menuSubItem("Cumulative", tabName = "cumulative",
                         icon = icon("angles-right")),
             menuSubItem("Monthly Means", tabName = "monthly_means",
                         icon = icon("angles-right"))),
    menuItem("Annual Statistics", tabName = "annual",
             icon = icon("calendar"),
             menuSubItem("Summary Statistics", tabName = "annual_stats",
                         icon = icon("angles-right")),
             menuSubItem("Mean Annual Discharge", tabName = "annual_means",
                         icon = icon("angles-right")),
             menuSubItem("Maximums & Minimums", tabName = "annual_extremes",
                         icon = icon("angles-right")),
             # menuSubItem("Low Flows",  tabName = "low_flows"),
             # menuSubItem("High Flows", tabName = "high_flows"),
             menuSubItem("Timing of Flows", tabName = "flow_timing",
                         icon = icon("angles-right")),
             menuSubItem("Normal Days", tabName = "normal_days",
                         icon = icon("angles-right")),
             menuSubItem("Total Discharge", tabName = "annual_totals",
                         icon = icon("angles-right"))),
    # menuItem("Flow Duration and Percentiles", tabName = "flows",
    #          icon = icon("clock")),
    menuItem("Analyses", tabName = "analyses",
             icon = icon("chart-line"),
             menuSubItem("Flow Duration & Percentiles", tabName = "flows",
                         icon = icon("angles-right")),
             menuSubItem("Annual Trends", tabName = "annual_trends",
                         icon = icon("angles-right")),
             menuSubItem("Volume Frequency", tabName = "volume_freq",
                         icon = icon("angles-right")),
             menuSubItem("HYDAT Peak Frequency", tabName = "hydat_peak",
                         icon = icon("angles-right")))
  )
}

footer_fasstr <- function() {
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
}
