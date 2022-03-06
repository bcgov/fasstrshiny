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

# Home -------------------------
ui_home <- fluidRow(
  column(
    width = 12, h2("Welcome to fasstrshiny"),

    tabBox(
      width = 12, height = min_height,

      # Disclaimer
      tabPanel(
        title = "Disclaimer", width = 12,
        p("This is an R Shiny app offering a user interface to the fasstr R package.",
          "To get started, first go to the Data tab on the Navigation menu, ",
          "choose a HYDAT station and load some data!"),
        p(strong("(Note that loading CSV data is not yet implemented)")),
        br(),
        p("Once you have loaded data you'll be able to explore the other tabs."),
        p("Remeber that this is a work in progress so keep track of what you like,",
          "don't like and what broke so we can make it better!")
      ),

      # Overview
      tabPanel(
        title = "Overview", width = 12,
        p("Blah, blah, blah")
      ),

      # R Workflow - Setup
      tabPanel(
        title = "Getting Setup", width = 12,
        p("Blah, blah, blah")
      ),
    )
  )
)

# Data -------------------------

## Data Loading ------------------
ui_data_load <- fluidRow(
  column(
    width = 12, h2("Loading Data"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, ",
               "what it does and how to use it"),
      radioGroupButtons(inputId = "data_source",
                        label = "Source", choices = c("HYDAT", "CSV"),
                        justified = TRUE,
                        selected = "HYDAT"),

      conditionalPanel(
        "input.data_source == 'HYDAT'",
        textInput("data_station_num", label = "Station Number",
                  value = "08HB048",
                  placeholder = "type station number or select from map")),

      conditionalPanel(
        "input.data_source != 'HYDAT'",
        fileInput("data_file", label = "Select File",
                  accept=c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))),
      bsButton("data_load", "Load Data", style = "primary"),
      hr(),

      show("data_show_stn", "Station Information"),
      fluidRow(id = "data_stn",
               column(
                 width = 6,
                 textInput('data_station_name',
                           label = "Name",
                           placeholder = "ex. Mission Creek")),
               column(
                 width = 6,
                 numericInput("data_basin_area",
                              label = html("Basin area (km<sup>2</sup>)"), value = 0,
                              min = 0, step = 0.1))),

      show("data_show_dates", "Dates"),
      div(id = "data_dates",
          uiOutput("ui_data_water_year"),
          uiOutput("ui_data_years_range"),
          uiOutput("ui_data_years_exclude"),
          uiOutput("ui_data_months")),

      show("data_show_types", "Data types"),
      div(id = "data_types",
          build_ui(id = "data", define_options = TRUE,
                   include = c("rolling", "discharge",
                               "missing", "allowed")))
    ),

    tabBox(
      id = "data_tabs", width = 9, height = min_height,

      ### HYDAT Map --------
      tabPanel(
        title = "HYDAT Map", width = 12,
        leafletOutput("data_hydat_map", width = "100%", height = "500px")
      ),

      ### HYDAT Table --------
      tabPanel(
        title = "HYDAT Table", width = 12,
        DTOutput("data_hydat_table")
      ),

      ### Plot --------
      tabPanel(
        title = "Plot", value = "data_tabs_plot",
        uiOutput("ui_data_plot_options", align = "right"),
        withSpinner(plotlyOutput('data_plot'))
      ),

      ### Table --------
      tabPanel(
        title = "Table",
        DTOutput("data_table")
      ),

      ### R Code -----------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("data_code")
      )
    )
  )
)


## Data Availability ---------------------
ui_data_available <- fluidRow(
  column(
    width = 12, h2("Data Availability"),
    tabBox(
      width = 12, height = min_height,

      #helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      ### Summary Plot -----------------
      tabPanel(
        title = "Data Summary Plot",
        selectizeInput("available_summary", "Statistic to explore",
                    c("Mean","Maximum","Minimum","StandardDeviation")),
        girafeOutput("available_plot1", height = "425px")
      ),

      ### Availability Plot -----------------
      tabPanel(
        title = "Data Availability Plot",
        fluidRow(
          column(width = 1,
                 checkboxGroupButtons(
                   "available_months",
                   label = "Months",
                   choices = list("Jan" = 1, "Feb" = 2,
                                  "Mar" = 3, "Apr" = 4,
                                  "May" = 5, "Jun" = 6,
                                  "Jul" = 7, "Aug" = 8,
                                  "Sep" = 9, "Oct" = 10,
                                  "Nov" = 11, "Dec" = 12),
                   selected = c(1:12),
                   direction = "vertical"),
                 bsTooltip("available_months",
                           "Months to include/exclude from the plot",
                           placement = "left"),
          ),
          column(width = 11, girafeOutput("available_plot2", height = "450px"))
        )
      ),

      ### Table -----------------
      tabPanel(
        title = "Table",
        DT::dataTableOutput("available_table")
      ),

      ### R Code -----------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("available_code")
      )
    )
  )
)


# Overview ---------------------------------------------------------------
ui_overview <- fluidRow(
  column(
    width = 12, h2("Overview"),
    box(width = 3,
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it")),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Overview")
    )
  )
)


# Hydrographs and Long-term -------------------------------------------------

ui_hydro <- fluidRow(
  column(
    width = 12, h2("Hydrographs and Long-term Stats"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      radioGroupButtons("hydro_type",
                        label = "Summary type", size = "sm",
                        choices = list("Daily",
                                       "Long-term Daily",
                                       "Long-term Monthly"),
                        selected = "Daily",
                        status = "primary", justified = TRUE),
      bsTooltip("hydro_type", "Type of statistic to calculate",
                placement = "left"),
      selectizeInput("hydro_mad",
                  label = "Percent Mean Annual Discharge",
                  choices = c(1:99),
                  selected = c(1, 5, 50, 95, 99),
                  multiple = TRUE),
      bsTooltip("hydro_mad", tips$mad, placement = "left"),
      uiOutput("ui_hydro")
    ),
    tabBox(
      width = 9, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_hydro_plot_options", align = "right"),
        girafeOutput("hydro_plot", height = "450px")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        uiOutput("ui_hydro_table_options", align = "right"),
        DTOutput("hydro_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("hydro_code")
      )
    )
  )
)


# Flow ----------------
ui_flows <- fluidRow(
  column(
    width = 12, h2("Flow duration and percentiles"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      numericInput("flows_flow",
                   label = "Flow value for percentile",
                   value = 10, min = 0),
      bsTooltip("flows_flow", tips$flow, placement = "left"),
      uiOutput("ui_flows"),
    ),
    tabBox(
      width = 9, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot - Flow duration",
        uiOutput("ui_flows_plot_options", align = "right"),
        girafeOutput("flows_plot", height = "400px"),
        p(style = "margin-bottom: 20px"),
        h4("Percentile Rank of Flow"),
        textOutput("flows_perc")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table - Percentiles",
        uiOutput("ui_flows_table_options", align = "right"),
        DTOutput("flows_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("flows_code")
      )
    )
  )
)



# Cumulative -------------

ui_cumulative <- fluidRow(
  column(
    width = 12, h2("Cumulative Statistics"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      radioGroupButtons("cum_type",
                        label = "Cumulative type",
                        choices = list("Annual", "Monthly", "Daily"),
                        justified = TRUE, status = "primary"),
      bsTooltip("cum_type", "Type of cumulative statistics to calculate",
                placement = "left"),
      radioButtons("cum_discharge",
                   label = "Discharge type",
                   choices = list("Volumetric Discharge (m3)" = FALSE,
                                  "Runoff Yield (mm)" = TRUE)),
      bsTooltip("cum_discharge", tips$discharge, placement = "left"),
      uiOutput("ui_cum_seasons"),
    ),
    tabBox(
      width = 9, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_cum_plot_options", align = "right"),
        girafeOutput("cum_plot", height = "400px")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        uiOutput("ui_cum_table_options", align = "right"),
        DTOutput("cum_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("cum_code")
      )
    )
  )
)


# Annual Statistics -----------------------------------------------------

## Annual Means -----------------------
ui_as_means <- fluidRow(
  column(
    width = 12, h2("Annual Means"),
    tabBox(
      width = 12, height = min_height,
      #helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        girafeOutput("am_plot", height = "400px")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("am_code")
      )
    )
  )
)



## Flow timing ------------------------------
ui_as_flow_timing <- fluidRow(
  column(
    width = 12, h2("Flow Timing"),
    box(
      width = 12,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      div(style = "max-width: 300px;",
          selectizeInput("ft_percent",
                      label = "Percents of total annual flows",
                      choices = c(1:99),
                      selected = c(25, 33, 50, 75),
                      multiple = TRUE),
          bsTooltip("ft_percent", tips$percent, placement = "left")
      ),

      tabBox(
        width = 12, height = min_height,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          girafeOutput("ft_plot", height = "400px")
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DTOutput("ft_table")
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("ft_code")
        )
      )
    )
  )
)

## Low flows ------------------------
ui_as_low_flows <- fluidRow(
  column(
    width = 12, h2("Low Flows"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      select_rolling("lf", set = FALSE, multiple = TRUE)
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        girafeOutput("lf_plot", height = "450px")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("lf_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("lf_code")
      )
    )
  )
)

## Peak flows ------------------------
ui_as_peak_flows <- fluidRow(
  column(
    width = 12, h2("Peak Flows"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      select_rolling("pf", set = FALSE)
    ),
    tabBox(
      width = 9, height = min_height,

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("pf_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("pf_code")
      )
    )
  )
)

## Days outside normal ------------------------
ui_as_outside_normal <- fluidRow(
  column(
    width = 12, h2("Days Outside Normal"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      sliderInput("on_normal", label = "Normal range ",
                  value = c(25, 75), min = 1, max = 99, step = 1),
      bsTooltip("on_normal", tips$normal, placement = "left")
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        girafeOutput("on_plot", height = "500px")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("on_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("on_code")
      )
    )
  )
)


# Analyses ---------------

## Annual Trends -----------------------
ui_analysis_annual <- fluidRow(
  column(
    width = 12, h2("Annual Trends"),
    box(width = 3,

        # Compute button
        bsButton("at_compute", "Compute Trends", style = "primary",
                 class = "centreButton"),
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        hr(class = "narrowHr"),

        # Other options
        uiOutput("ui_at_exclude"),

        fluidRow(
          column(width = 6,
                 awesomeRadio("at_zyp",
                              label = "Trend method",
                              choices = list("Zhang" = "zhang",
                                             "Yue-Pilon" = "yuepilon"),
                              selected = "zhang")),
          column(width = 6,
                 numericInput("at_alpha", label = "Trend alpha",
                              value = 0.05, min = 0, max = 0.3, step = 0.05))
        ),
        bsTooltip("at_zyp", tips$zyp, placement = "left"),
        bsTooltip("at_alpha", tips$alpha, placement = "left"),

        fluidRow(
          column(6, selectizeInput("at_annual_percentiles",
                                   label = "Annual perc.",
                                   choices = c(1:99),
                                   selected = c(10,90),
                                   multiple = TRUE)),
          column(6, selectizeInput("at_monthly_percentiles",
                                   label = "Monthly perc.",
                                   choices = c(1:99),
                                   selected = c(10,20),
                                   multiple = TRUE))
        ),
        bsTooltip("at_annual_percentiles", tips$percentiles,
                  placement = "left"),
        bsTooltip("at_monthly_percentiles", tips$percentiles,
                  placement = "left"),

        strong("Low Flows"),
        select_rolling("at_low", set = FALSE, multiple = TRUE),

        selectizeInput("at_percent",
                       label = "Percents of total annual flows",
                       choices = c(1:99),
                       selected = c(25, 33, 50, 75),
                       multiple = TRUE),
        bsTooltip("at_percent", tips$percent, placement = "left"),

        sliderInput("at_normal", label = "Days Outside Normal - Range",
                    value = c(25, 75), min = 1, max = 99, step = 1),
        bsTooltip("at_normal", tips$normal, placement = "left"),

        uiOutput("ui_at_allowed")
    ),

    tabBox(
      width = 9, height = min_height,

      ### Plot/Table ---------------------
      tabPanel(
        title = "Explore statistics",
        withSpinner(DTOutput("at_table_fit")),
        p(style = "margin-bottom:30px"), # A bit of space
        fluidRow(
          column(4, gt_output("at_table_years_sub")),
          column(8,
                 conditionalPanel(
                   "output.at_plot",
                   helpText("Click on a point to add year to ",
                            "'Years to exclude'. Remember to re-Compute ",
                            "Trends.")),
                 girafeOutput("at_plot", height = "450px")))
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table - Yearly stats",
        withSpinner(DTOutput("at_table_years"))
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("at_code")
      )
    )
  )
)

## Volume Frequency - High/Low ------------------
ui_analysis_volume_freq <- fluidRow(
  column(
    width = 12, h2("High/Low Volume Frequency Analysis"),
    box(width = 3,

        # Buttons
        bsButton("vf_compute", "Compute Analysis", style = "primary",
                 class = "centreButton"),
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        hr(class = "narrowHr"),

        # Other
        uiOutput("ui_vf_exclude"),
        select_rolling("vf", set = FALSE, multiple = TRUE),

        fluidRow(
          column(
            width = 6,
            awesomeRadio("vf_use_max",
                         label = "Flow type",
                         choices = list("Low" = FALSE,
                                        "High" = TRUE),
                         selected = FALSE)),
          column(
            width = 6,
            awesomeRadio("vf_fit_distr",
                         label = "Distribution",
                         choices = list("PIII" = "PIII",
                                        "Weibull" = "weibull")))
        ),
        bsTooltip("vf_use_max", tips$use_max, placement = "left"),
        bsTooltip("vf_fit_distr", tips$fit_distr, placement = "left"),

        selectizeInput(
          "vf_fit_quantiles",
          label = "Quantiles to estimate",
          choices = seq(0.01, 0.999, 0.0025),
          selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                       0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
          multiple = TRUE),
        bsTooltip("vf_fit_quantiles", tips$fit_quantiles, placement = "left"),

        fluidRow(
          column(
            width = 6,
            materialSwitch("vf_plot_curve",
                           label = tags$span("Plot curve",
                                             id = "vf_plot_curve_tip"),
                           value = TRUE,
                           status = "success")),
          column(
            width = 6,
            materialSwitch("vf_use_log",
                           label = tags$span("Log trans",
                                             id = "vf_use_log_tip"),
                           value = FALSE, status = "success"))
        ),
        bsTooltip("vf_plot_curve_tip", tips$plot_curve, placement = "left"),
        bsTooltip("vf_use_log_tip", tips$use_log, placement = "left"),

        fluidRow(
          column(6,
                 awesomeRadio("vf_prob_plot",
                              label = "Plotting positions",
                              choices = list("Weibull" = "weibull",
                                             "Median" = "median",
                                             "Hazen" = "hazen"))),
          column(6, textInput(
            "vf_prob_scale",
            label = "Probabilies to plot",
            value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001"))
        ),
        bsTooltip("vf_prob_plot", tips$prob_plot, placement = "left"),
        bsTooltip("vf_prob_scale", tips$prob_scale, placement = "left")
    ),

    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        conditionalPanel(
          "output.vf_plot",
          helpText("Click on a point to add that year to ",
                   "'Years to exclude'. Remember to re-Compute ",
                   "Analysis.")),
        withSpinner(girafeOutput("vf_plot"))
      ),

      ### Table - Plot Data ---------------------
      tabPanel(
        title = "Table - Plot Data",
        withSpinner(DTOutput("vf_table_plot"))
      ),

      ### Table - Fitted Quantiles ---------------------
      tabPanel(
        title = "Table - Fitted Quantiles",
        withSpinner(DTOutput("vf_table_fit"))
      ),

      ### Plot ---------------------
      tabPanel(
        title = "Fit Checks",
        uiOutput("ui_vf_day"),
        verbatimTextOutput("vf_fit_stats"),
        withSpinner(plotOutput("vf_fit_plot", height = "550px"))
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("vf_code")
      )
    )
  )
)

## Volume Frequency - HYDAT Peaks ------------------
ui_analysis_hydat_peak <- fluidRow(
  column(
    width = 12, h2("HYDAT Peak Volume Frequency Analysis"),
    box(width = 3,

        bsButton("hp_compute", "Compute Analysis", style = "primary",
                 class = "centreButton"),
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        hr(class = "narrowHr"),

        fluidRow(
          column(width = 6,
                 awesomeRadio("hp_use_max",
                              label = "Flow type",
                              choices = list("Low" = FALSE,
                                             "High" = TRUE),
                              selected = FALSE)),
          column(width = 6,
                 awesomeRadio("hp_fit_distr",
                              label = "Distribution",
                              choices = list("PIII" = "PIII",
                                             "Weibull" = "weibull")))
        ),
        bsTooltip("hp_use_max", tips$use_max, placement = "left"),
        bsTooltip("hp_fit_distr", tips$fit_distr, placement = "left"),

        selectizeInput(
          "hp_fit_quantiles",
          label = "Quantiles to estimate",
          choices = seq(0.01, 0.999, 0.0025),
          selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                       0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
          multiple = TRUE),
        bsTooltip("hp_fit_quantiles", tips$fit_quantiles, placement = "left"),

        fluidRow(
          column(width = 6,
                 materialSwitch("hp_plot_curve", label = "Plot curve", value = TRUE,
                                status = "success")),
          column(width = 6,
                 materialSwitch("hp_use_log",
                                label = "Log trans",
                                value = FALSE, status = "success"))
        ),
        bsTooltip("hp_plot_curve", tips$plot_curve, placement = "left"),
        bsTooltip("hp_use_log", tips$use_log, placement = "left"),

        fluidRow(
          column(6, awesomeRadio("hp_prob_plot",
                                 label = "Plotting positions",
                                 choices = list("Weibull" = "weibull",
                                                "Median" = "median",
                                                "Hazen" = "hazen"))),
          column(6, textInput(
            "hp_prob_scale",
            label = "Probabilies to plot",
            value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001"))
        ),
        bsTooltip("hp_prob_plot", tips$prob_plot, placement = "left"),
        bsTooltip("hp_prob_scale", tips$prob_scale, placement = "left"),
    ),

    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        withSpinner(girafeOutput("hp_plot"))
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        withSpinner(DTOutput("hp_table"))
      ),

      ### Plot ---------------------
      tabPanel(
        title = "Fit Checks",
        verbatimTextOutput("hp_fit_stats"),
        withSpinner(plotOutput("hp_fit_plot", height = "550px"))
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("hp_code")
      )
    )
  )
)







# Combine -------------------------------------------------------------------
tagList(
  dashboardPage(
    dashboardHeader(title = "fasstr Shiny"),

    ## Sidebar ----------
    dashboardSidebar(
      tags$script(src = "tips.js"),
      sidebarMenu(
        id = "menu",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data", tabName = "data", icon = icon("table"),
                 menuSubItem("Loading", tabName = "data_load"),
                 menuSubItem("Availability", tabName = "data_available")),
        menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
        menuItem("Hydrographs & Long-term Stats", tabName = "hydro",
                 icon=icon("chart-bar")),
        menuItem("Cumulative Hydrographs", tabName = "cumulative",
                 icon = icon("chart-area")),
        menuItem("Flow duration and percentiles", tabName = "flows"),
        menuItem("Annual Statistics", tabName = "annual",
                 icon = icon("calendar"),
                 menuSubItem("Annual Means", tabName = "as_means"),
                 menuSubItem("Flow timing", tabName = "as_flow_timing"),
                 menuSubItem("Low Flows", tabName = "as_low_flows"),
                 menuSubItem("Peak Flows", tabName = "as_peak_flows"),
                 menuSubItem("Days outside normal", tabName = "as_outside_normal")),
        menuItem("Analyses", tabName = "analyses",
                 icon = icon("chart-line"),
                 menuSubItem("Annual Trends", tabName = "analysis_annual"),
                 menuSubItem("Volume Frequency", tabName = "analysis_volume_freq"),
                 menuSubItem("HYDAT Peak", tabName = "analysis_hydat_peak"))
      ),
      div(style = "margin-top: 10px", gt_output("data_info"))
    ),
    ## Body -----------------
    dashboardBody(
      useShinyjs(),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bcgov.css")),
      tabItems(
        tabItem("home", ui_home),
        tabItem("data_load", ui_data_load),
        tabItem("data_available", ui_data_available),
        tabItem("overview", ui_overview),
        tabItem("hydro", ui_hydro),
        tabItem("cumulative", ui_cumulative),
        tabItem("flows", ui_flows),
        tabItem("as_means", ui_as_means),
        tabItem("as_flow_timing", ui_as_flow_timing),
        tabItem("as_low_flows", ui_as_low_flows),
        tabItem("as_peak_flows", ui_as_peak_flows),
        tabItem("as_outside_normal", ui_as_outside_normal),
        tabItem("analysis_annual", ui_analysis_annual),
        tabItem("analysis_volume_freq", ui_analysis_volume_freq),
        tabItem("analysis_hydat_peak", ui_analysis_hydat_peak)
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

