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
  column(width = 12)
)

# Settings ---------------------
ui_settings <- fluidRow(
  column(
    width = 12, h2("Settings"),
    box(title = "Global Settings", width = 6,
        p("Set once here, apply to whole app"),

        build_ui(id = "opts", define_options = TRUE,
                 include = c("rolling", "months"))),

    box(title = "Default Settings", width = 6,
        p("Set defaults here, but can change throughout the app as needed"),

        build_ui(id = "opts", define_options = TRUE,
                 include = c("discharge", "missing", "allowed", "percentiles",
                             "custom_months"))
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

      conditionalPanel(
        "input.data_source != 'HYDAT'",
        h4("Station Information"),
        fluidRow(column(width = 6,
                        textInput('data_station_name',
                                  label = "Station/stream name:",
                                  placeholder = "ex. Mission Creek")),
                 column(width = 6,
                        numericInput("data_basin_area",
                                     label = "Basin area (sq. km):", value = 0,
                                     min = 0, step = 0.1)))),

      uiOutput("ui_data_water_year"),
      uiOutput("ui_data_years_range"),
      uiOutput("ui_data_years_exclude")
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
        title = "Plot", value = "data_plot",
        uiOutput("ui_data_plot_options", align = "right"),
        plotlyOutput('data_plot')
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


## Data Screening ---------------------
ui_data_screen <- fluidRow(
  column(
    width = 12, h2("Data Screening"),
    tabBox(
      width = 12, height = min_height,

      ### Summary Plot -----------------
      tabPanel(
        title = "Data Summary Plot",
        selectInput("screen_summary", "Review annual daily metric",
                    c("Mean","Maximum","Minimum","StandardDeviation")),
        girafeOutput("screen_plot1", height = "450px")
      ),

      ### Availability Plot -----------------
      tabPanel(
        title = "Data Availability Plot",
        fluidRow(
          column(width = 1,
                 checkboxGroupButtons(
                   "screen_months",
                   label = "Months",
                   choices = list("Jan" = 1, "Feb" = 2,
                                  "Mar" = 3, "Apr" = 4,
                                  "May" = 5, "Jun" = 6,
                                  "Jul" = 7, "Aug" = 8,
                                  "Sep" = 9, "Oct" = 10,
                                  "Nov" = 11, "Dec" = 12),
                   selected = c(1:12),
                   direction = "vertical")
          ),
          column(width = 11, girafeOutput("screen_plot2", height = "450px"))
        )
      ),

      ### Table -----------------
      tabPanel(
        title = "Table",
        DT::dataTableOutput("screen_table")
      ),

      ### R Code -----------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("screen_code")
      )
    )
  )
)


# Stats Summary ---------------------------------------------------------------

## General ----------------
ui_sum_general <- fluidRow(
  column(
    width = 12, h2("General Summary Statistics"),
    box(
      width = 3,
      radioGroupButtons("sum_type",
                        label = "Summary type",
                        choices = list("Long-term", "Annual",
                                       "Monthly", "Daily"),
                        justified = TRUE, direction = "vertical"),
      selectInput("sum_mad",
                  label = "Mean Annual Discharge percentiles",
                  choices = c(1:99),
                  selected = c(1, 5, 50, 95, 99),
                  multiple = TRUE),
      uiOutput("ui_sum"),
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_sum_plot_options", align = "right"),
        h4("ggiraph example"),
        girafeOutput("sum_plot", height = "450px"),
        h4("plotly example"),
        plotlyOutput("sum_plot_test", height = "450px")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        uiOutput("ui_sum_table_options", align = "right"),
        DTOutput("sum_table")
      ),

      ### MAD --------------------
      tabPanel(
        title = "MAD",
        h4("Mean Annual Discharge (MAD)"),
        gt_output("sum_mad")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("sum_code")
      )
    )
  )
)


## Flow ----------------
ui_sum_flow <- fluidRow(
  column(
    width = 12, h2("Flow duration and percentiles"),
    box(
      width = 3,
      numericInput("sumfl_flow",
                   label = "Flow value for percentile",
                   value = 10, min = 0),
      uiOutput("ui_sumfl"),
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot - Flow duration",
        uiOutput("ui_sumfl_plot_options", align = "right"),
        plotOutput("sumfl_plot"),
        p(style = "margin-bottom: 20px"),
        h4("Percentile Rank of Flow"),
        textOutput("sumfl_perc")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table - Percentiles",
        uiOutput("ui_sumfl_table_options", align = "right"),
        DTOutput("sumfl_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("sumfl_code")
      )
    )
  )
)


## Annual Means -----------------------
ui_sum_annual <- fluidRow(
  column(
    width = 12, h2("Annual Means"),
    tabBox(
      width = 12, height = min_height,
      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_sumam"),
        plotOutput("sumam_plot")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("sumam_code")
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
      radioGroupButtons("cum_type",
                        label = "Cumulative type",
                        choices = list("Annual", "Monthly", "Daily"),
                        justified = TRUE),
      radioButtons("cum_discharge",
                   label = "Discharge type",
                   choices = list("Volumetric Discharge (m3)" = FALSE,
                                  "Runoff Yield (mm)" = TRUE)),
      materialSwitch("cum_seasons",
                     label = "Include seasons",
                     value = TRUE)
      ),
    tabBox(
      width = 9, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_cum_plot_options", align = "right"),
        plotOutput("cum_plot", height = "500px")
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


# Annual Hydrograph Stats -----------------------------------------------------

## Flow timing ------------------------------
ui_ah_flow_timing <- fluidRow(
  column(
    width = 12, h2("Flow Timing"),
    box(
      width = 12,
      div(style = "max-width: 300px;",
          selectInput("ahft_percent",
                      label = "Percents of total annual flows",
                      choices = c(1:99),
                      selected = c(25, 33, 50, 75),
                      multiple = TRUE)),

      tabBox(
        width = 12, height = min_height,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          plotOutput("ahft_plot")
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DTOutput("ahft_table")
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("ahft_code")
        )
      )
    )
  )
)

## Low flows ------------------------
ui_ah_low_flows <- fluidRow(
  column(
    width = 12, h2("Low Flows"),
    box(
      width = 3,
      selectInput("ahlf_roll",
                  label = "Days to calculate rolling averages over",
                  choices = c(1:31),
                  selected = c(1, 3, 7, 30),
                  multiple = TRUE),
      uiOutput("ui_ahlf")
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        plotOutput("ahlf_plot")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahlf_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahlf_code")
      )
    )
  )
)

## Flow peak ------------------------
ui_ah_peak <- fluidRow(
  column(
    width = 12, h2("Peak Flows"),
    box(
      width = 3,
      selectInput("ahp_roll",
                  label = "Days to calculate rolling averages over",
                  choices = c(1:31),
                  selected = 1),
      uiOutput("ui_ahp")
    ),
    tabBox(
      width = 9, height = min_height,

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahp_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahp_code")
      )
    )
  )
)

## Days outside normal ------------------------
ui_ah_outside_normal <- fluidRow(
  column(
    width = 12, h2("Days Outside Normal"),
    box(
      width = 3,
      selectInput("ahon_percentiles",
                  label = "Normal range (percentiles)",
                  choices = c(1:99),
                  selected = c(25, 75),
                  multiple = TRUE),
      uiOutput("ui_ahon")
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        plotOutput("ahon_plot", height = "550px")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahon_table")
      ),

      ### R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahon_code")
      )
    )
  )
)


# Computations ---------------

## Annual Trends -----------------------
ui_comp_annual <- fluidRow(
  column(
    width = 12, h2("Annual Trends"),
    box(width = 3,

        # Compute button
        bsButton("at_compute", "Compute Trends", style = "primary",
                 class = "centreButton"),
        hr(class = "narrowHr"),

        # Other options
        fluidRow(
          uiOutput("ui_at_exclude"),
          column(width = 6,
                 awesomeRadio("at_zyp",
                              label = "Trend method",
                              choices = list("Zhang" = "zhang",
                                             "Yue-Pilon" = "yuepilon"),
                              selected = "zhang")),
          column(width = 6,
                 numericInput("at_alpha", label = "Trend Alpha",
                              value = 0.05, min = 0, max = 0.3, step = 0.05))),
        strong("Percentiles"),
        fluidRow(
          column(6, selectizeInput("at_annual_percentiles",
                                   label = "Annual",
                                   choices = c(1:99),
                                   selected = c(10,90),
                                   multiple = TRUE)),
          column(6, selectizeInput("at_monthly_percentiles",
                                   label = "Monthly",
                                   choices = c(1:99),
                                   selected = c(10,20),
                                   multiple = TRUE))),

        strong("Low Flows"),
        fluidRow(
          column(6, selectizeInput("at_low_roll_days",
                                   label = "Rolling days",
                                   choices = c(1:180),
                                   selected = c(1,3,7,30),
                                   multiple = TRUE)),
          column(6,selectInput("at_low_roll_align", label = "Rolling alignment",
                               choices = list("Right" = "right",
                                              "Left" = "left",
                                              "Center" = "center"),
                               selected = "right"))),

        selectInput("at_percent",
                    label = "Flow Timing: No. days for rolling",
                    choices = c(1:31),
                    selected = c(1, 3, 7, 30),
                    multiple = TRUE),

        sliderInput("at_normal", label = "Normal Days: Percentile range",
                    value = c(25, 75), min = 1, max = 99, step = 1),

        uiOutput("ui_at_allowed")
    ),

    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Explore statistics",
        DTOutput("at_table_fit"),
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
        DTOutput("at_table_years")
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
ui_comp_volume_freq <- fluidRow(
  column(
    width = 12, h2("High/Low Volume Frequency Analysis"),
    box(width = 3,

        # Buttons
        bsButton("vf_compute", "Compute Analysis", style = "primary",
                 class = "centreButton"),
        hr(class = "narrowHr"),

        # Other
        uiOutput("ui_vf_exclude"),
        selectInput("vf_roll_extra",
                    label = "Days to calculate rolling averages over",
                    choices = c(1:31),
                    selected = c(1, 3, 7, 30),
                    multiple = TRUE),
        uiOutput("ui_vf"),

        fluidRow(
          column(
            width = 6,
            radioGroupButtons("vf_use_max",
                              label = "Low or High Flow",
                              choices = list("Low" = FALSE,
                                             "High" = TRUE),
                              selected = FALSE,
                              justified = TRUE)),
          column(
            width = 6,
            materialSwitch("vf_log",
                           label = "Log transform data",
                           value = FALSE))
        ),

        fluidRow(
          column(6, radioGroupButtons("vf_fit_distr",
                                      label = "Distribution used to fit data",
                                      choices = list("PIII" = "PIII",
                                                     "Weibull" = "weibull"))),
          column(6, selectizeInput(
            "vf_quantiles",
            label = "Quantiles to estimate",
            choices = seq(0.01, 0.999, 0.0025),
            selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                         0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
            multiple = TRUE))),

        materialSwitch("vf_plot_curve", label = "Plot curve", value = TRUE),

        fluidRow(
          column(6, radioGroupButtons("vf_prob_plot",
                                      label = "Plotting positions",
                                      choices = list("Weibull" = "weibull",
                                                     "Median" = "median",
                                                     "Hazen" = "hazen"))),
          column(6, textInput("vf_prob_scale",
                              label = "Probabilies to plot",
                              value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001")))
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
        girafeOutput("vf_plot")
      ),

      ### Table - Plot Data ---------------------
      tabPanel(
        title = "Table - Plot Data",
        DTOutput("vf_table_plot")
      ),

      ### Table - Fitted Quantiles ---------------------
      tabPanel(
        title = "Table - Fitted Quantiles",
        DTOutput("vf_table_fit")
      ),

      ### Plot ---------------------
      tabPanel(
        title = "Fit Checks",
        uiOutput("ui_vf_day"),
        verbatimTextOutput("vf_fit_stats"),
        plotOutput("vf_fit_plot", height = "550px")
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
ui_comp_hydat_peak <- fluidRow(
  column(
    width = 12, h2("HYDATE Peak Volume Frequency Analysis"),
    box(width = 3,

        bsButton("hp_compute", "Compute Analysis", style = "primary",
                 class = "centreButton"),
        hr(class = "narrowHr"),

        fluidRow(
          column(
            width = 6,
            radioGroupButtons("hp_use_max",
                              label = "Low or High Flow",
                              choices = list("Low" = FALSE,
                                             "High" = TRUE),
                              selected = FALSE,
                              justified = TRUE)),
          column(
            width = 6,
            materialSwitch("hp_log",
                           label = "Log transform data",
                           value = FALSE))
        ),

        fluidRow(
          column(6, radioGroupButtons("hp_fit_distr",
                                      label = "Distribution used to fit data",
                                      choices = list("PIII" = "PIII",
                                                     "Weibull" = "weibull"))),
          column(6, selectizeInput(
            "hp_quantiles",
            label = "Quantiles to estimate",
            choices = seq(0.01, 0.999, 0.0025),
            selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                         0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
            multiple = TRUE))),

        materialSwitch("hp_plot_curve", label = "Plot curve", value = TRUE),

        fluidRow(
          column(6, radioGroupButtons("hp_prob_plot",
                                      label = "Plotting positions",
                                      choices = list("Weibull" = "weibull",
                                                     "Median" = "median",
                                                     "Hazen" = "hazen"))),
          column(6, textInput("hp_prob_scale",
                              label = "Probabilies to plot",
                              value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001")))
    ),

    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        plotOutput("hp_plot")
      ),

      ### Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("hp_table")
      ),

      ### Plot ---------------------
      tabPanel(
        title = "Fit Checks",
        verbatimTextOutput("hp_fit_stats"),
        plotOutput("hp_fit_plot", height = "550px")
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
    dashboardSidebar(
      tags$script(src = "tips.js"),
      useShinyjs(),
      sidebarMenu(
        id = "menu",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Settings", tabName = "settings", icon = icon("cog")),
        menuItem("Data", tabName = "data", icon = icon("table"),
                 menuSubItem("Loading", tabName = "data_load"),
                 menuSubItem("Screening", tabName = "data_screen")),
        menuItem("Summary statistics", tabName = "summary",
                 icon=icon("chart-bar"),
                 menuSubItem("General", tabName = "sum_general"),
                 menuSubItem("Flow duration and percentiles",
                             tabName = "sum_flow"),
                 menuSubItem("Annual Means", tabName = "sum_annual")),
        menuItem("Cumulative Stats", tabName = "cumulative",
                 icon = icon("chart-area")),
        menuItem("Annual Hydrograph Stats", tabName = "annual",
                 icon = icon("calendar"),
                 menuSubItem("Flow timing", tabName = "ah_flow_timing"),
                 menuSubItem("Low Flows", tabName = "ah_low_flows"),
                 menuSubItem("Peak Flows", tabName = "ah_peak"),
                 menuSubItem("Days outside normal", tabName = "ah_outside_normal")),
        menuItem("Computations", tabName = "computed",
                 icon = icon("chart-line"),
                 menuSubItem("Annual Trends", tabName = "comp_annual"),
                 menuSubItem("Volume Frequency", tabName = "comp_volume_freq"),
                 menuSubItem("HYDAT Peak", tabName = "comp_hydat_peak"))
      )
    ),
    dashboardBody(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bcgov.css")),
      tabItems(
        tabItem("home", ui_home),
        tabItem("settings", ui_settings),
        tabItem("data_load", ui_data_load),
        tabItem("data_screen", ui_data_screen),
        tabItem("sum_general", ui_sum_general),
        tabItem("sum_flow", ui_sum_flow),
        tabItem("sum_annual", ui_sum_annual),
        tabItem("cumulative", ui_cumulative),
        tabItem("ah_flow_timing", ui_ah_flow_timing),
        tabItem("ah_low_flows", ui_ah_low_flows),
        tabItem("ah_peak", ui_ah_peak),
        tabItem("ah_outside_normal", ui_ah_outside_normal),
        tabItem("comp_annual", ui_comp_annual),
        tabItem("comp_volume_freq", ui_comp_volume_freq),
        tabItem("comp_hydat_peak", ui_comp_hydat_peak)
      )
    )
  ),
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

