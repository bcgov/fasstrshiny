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
      width = 4,
      fluidRow(
        column(
          width = 5,
          radioGroupButtons(inputId = "data_source",
                            label = "Source", choices = c("HYDAT", "CSV File"),
                            selected = "HYDAT"),
          bsButton("data_load", "Load Data", style = "primary")),
        column(
          width = 7,
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
                               ".csv"))))),
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
      id = "data_tabs", width = 8, height = min_height,

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
        uiOutput("ui_data_plot_options"),
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
        plotlyOutput("screen_plot1")
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
          column(width = 11, plotlyOutput("screen_plot2"))
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


# Stats Summary - General ----------------
ui_sum_general <- fluidRow(
  column(
    width = 12, h2("General Summary Statistics"),
    box(
      width = 4,
      radioGroupButtons("sum_type",
                        label = "Summary type",
                        choices = list("Long-term", "Annual",
                                       "Monthly", "Daily"),
                        justified = TRUE),
      uiOutput("ui_sum"),
    ),
    tabBox(
      width = 8, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_sum_plot_options"),
        plotOutput("sum_plot")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        uiOutput("ui_sum_table_options"),
        DTOutput("sum_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("sum_code")
      )
    )
  )
)


# Stats Summary - Flow ----------------
ui_sum_flow <- fluidRow(
  column(
    width = 12, h2("Flow duration and percentiles"),
    box(
      width = 4,
      uiOutput("ui_sumfl"),
    ),
    tabBox(
      width = 8, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot - Flow duration",
        uiOutput("ui_sumfl_plot_options"),
        plotOutput("sumfl_plot")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table - Percentiles",
        uiOutput("ui_sumfl_table_options"),
        DTOutput("sumfl_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("sumfl_code")
      )
    )
  )
)

# Stats Summary - Single ----------------
ui_sum_single <- fluidRow(
  column(
    width = 12, h2("Long-term Single stats"),
    box(
      width = 4,
      selectInput("sumsi_mad",
                  label = "Mean Annual Discharge percentiles (MAD)",
                  choices = c(1:99),
                  selected = c(1, 5, 50, 95, 99),
                  multiple = TRUE),
      select_percentiles(id = "sumsi", set = FALSE),
      numericInput("sumsi_flow",
                   label = "Flow value for calculating percentile rank",
                   value = 10, min = 0),
      uiOutput("ui_sumsi"),
    ),
    tabBox(
      width = 8, height = min_height,

      ## Table ---------------------
      tabPanel(
        title = "Stats",
        h4("Mean Annual Discharge (MAD)"),
        gt_output("sumsi_mad"),
        h4("Long-term Percentiles"),
        gt_output("sumsi_perc"),
        h4("Percentile Rank of Flow"),
        gt_output("sumsi_flow")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("sumsi_code")
      )
    )
  )
)

# Cumulative -------------

ui_cumulative <- fluidRow(
  column(
    width = 12, h2("Cumulative Statistics"),
    box(
      width = 4,
      radioGroupButtons("cum_type",
                        label = "Cumulative type",
                        choices = list("Annual", "Monthly", "Daily"),
                        justified = TRUE),
      radioButtons("cum_discharge",
                   label = "Discharge type",
                   choices = list("Volumetric Discharge (m3)" = FALSE,
                                  "Runoff Yield (mm)" = TRUE)),
      checkboxInput("cum_seasons",
                    label = "Include seasons",
                    value = TRUE)
      ),
    tabBox(
      width = 8, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_cum_plot_options"),
        plotOutput("cum_plot", height = "500px")
        ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        uiOutput("ui_cum_table_options"),
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


# AH - Flow timing ------------------------------
ui_ah_flow_timing <- fluidRow(
  column(
    width = 12, h2("Flow Timing"),
    box(
      width = 4,
      selectInput("ahft_percent",
                  label = "Percents of total annual flows",
                  choices = c(1:99),
                  selected = c(25, 33, 50, 75),
                  multiple = TRUE)
    ),
    tabBox(
      width = 8, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        plotOutput("ahft_plot")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahft_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahft_code")
      )
    )
  )
)

# AH - Low flows ------------------------
ui_ah_low_flows <- fluidRow(
  column(
    width = 12, h2("Low Flows"),
    box(
      width = 4,
      selectInput("ahlf_roll",
                  label = "Days to calculate rolling averages over",
                  choices = c(1:31),
                  selected = c(1, 3, 7, 30),
                  multiple = TRUE),
      uiOutput("ui_ahlf")
    ),
    tabBox(
      width = 8, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        plotOutput("ahlf_plot")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahlf_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahlf_code")
      )
    )
  )
)

# AH - Flow peak ------------------------
ui_ah_peak <- fluidRow(
  column(
    width = 12, h2("Peak Flows"),
    box(
      width = 4,
      "SHOULD THIS BE HERE? OR LEAVE AS DEFAULT GLOBAL?",
      selectInput("ahp_roll",
                  label = "Days to calculate rolling averages over",
                  choices = c(1:31),
                  selected = 1),
      uiOutput("ui_ahp")
    ),
    tabBox(
      width = 8, height = min_height,

      ## Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahp_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahp_code")
      )
    )
  )
)

# AH - Days outside normal ------------------------
ui_ah_outside_normal <- fluidRow(
  column(
    width = 12, h2("Days Outside Normal"),
    box(
      width = 4,
      selectInput("ahon_percentiles",
                  label = "Normal range (percentiles)",
                  choices = c(1:99),
                  selected = c(25, 75),
                  multiple = TRUE),
      uiOutput("ui_ahon")
    ),
    tabBox(
      width = 8, height = min_height,

      ## Plot ---------------------
      tabPanel(
        title = "Plot",
        plotOutput("ahon_plot")
      ),

      ## Table ---------------------
      tabPanel(
        title = "Table",
        DTOutput("ahon_table")
      ),

      ## R Code ---------------------
      tabPanel(
        title = "R Code",
        verbatimTextOutput("ahon_code")
      )
    )
  )
)


# Stats Computed ---------------

## Annual Trends -----------------------
ui_comp_annual <- fluidRow(
  column(
    width = 12, h2("Annual Trends"),
    box(width = 3,
        actionButton("trends_compute", "Compute Trends"),
        helpText("Trending Methods"),
        selectInput("trends_zyp_method", label = "Pre-whitened trend method:",
                    choices = list("Zhang" = "zhang", "Yue-Pilon" = "yuepilon"),
                    selected = "zhang"),
        numericInput("trends_alpha", label = "Alpha for plotting:", value = 0.05, min = 0, max = 1, step = 0.01),
        helpText("Summary Statistics"),
        fluidRow(column(6,numericInput("trends_roll_days",
                                       label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
                 column(6,selectInput("trends_roll_align", label = "Rolling alignment:",
                                      choices = list("Right" = "right", "Left" = "left", "Center" = "center"),
                                      selected = "Right"))),
        fluidRow(column(6, selectizeInput("trends_ann_ptiles",
                                          label = "Annual percentiles:",
                                          choices = c(1:99),
                                          selected = c(10,90),
                                          multiple = TRUE)),
                 column(6, selectizeInput("trends_mon_ptiles",
                                          label = "Monthly percentiles:",
                                          choices = c(1:99),
                                          selected = c(10,20),
                                          multiple = TRUE))),
        helpText("Low Flows"),
        fluidRow(column(6, selectizeInput("trends_low_roll_days",
                                          label = "Rolling days:",
                                          choices = c(1:180),
                                          selected = c(1,3,7,30),
                                          multiple = TRUE)),
                 column(6,selectInput("trends_low_roll_align", label = "Rolling alignment:",
                                      choices = list("Right" = "right", "Left" = "left", "Center" = "center"),
                                      selected = "Right"))),
        helpText("Flow Timing"),
        selectizeInput("trends_timing",
                       label = "Percent of annual flow:",
                       choices = seq(0.1, 99.9, by = 0.1),
                       selected = c(25,33.3,50,75),
                       multiple = TRUE),
        helpText("Normal Days"),
        fluidRow(column(6,numericInput("trends_normal_lower",
                                       label = "Lower Limit:", value = 25, min = 1, max = 99, step = 1)),
                 column(6,numericInput("trends_normal_upper",
                                       label = "Upper Limit:", value = 75, min = 1, max = 99, step = 1))),
        checkboxInput("trends_ign_missing_box", "Calculate statistics despite missing values", value = FALSE)

    ),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Analysis",

        DT::dataTableOutput("trends_results"),
        textOutput("testing_rows"),

        fluidRow(column(3, DT::dataTableOutput("trends_results_data")),
                 column(9, plotlyOutput("trends_plot")))
      ),
      tabPanel(
        title = "R Code",
        h4("Copy and paste the following into an R console or script to reproduce the results."),

        htmlOutput("trends_code")
      )
    )
  )
)

## Flow Frequency ------------------

ui_comp_flow <- fluidRow(
  column(
    width = 12, h2("Flow Frequency"),
    box(width = 3,
        actionButton("freq_compute", "Compute Analysis"),
        h4("Data Selection"),
        # uiOutput("freq_station_num"),
        # selectInput("freq_year_start", label = "Calendar:", choices = list("Jan-Dec" = 1, "Feb-Jan" = 2,
        #                                                                    "Mar-Feb" = 3, "Apr-Mar" = 4,
        #                                                                    "May-Apr" = 5, "Jun-May" = 6,
        #                                                                    "Jul-Jun" = 7, "Aug-Jul" = 8,
        #                                                                    "Sep-Aug" = 9, "Oct-Sep" = 10,
        #                                                                    "Nov-Oct" = 11, "Dec-Nov" = 12),
        #             selected = 1),
        # uiOutput("freq_slider"),
        # uiOutput("freq_exclude"),
        selectizeInput("freq_months",
                       label = "Months:",
                       choices = list("Jan" = 1, "Feb" = 2,
                                      "Mar" = 3, "Apr" = 4,
                                      "May" = 5, "Jun" = 6,
                                      "Jul" = 7, "Aug" = 8,
                                      "Sep" = 9, "Oct" = 10,
                                      "Nov" = 11, "Dec" = 12),
                       selected = c(1:12),
                       multiple = TRUE),
        h4("Data Statistics"),
        selectInput("freq_use_max", label = "Annual Extreme:", choices = c("Minimum", "Maximum"), selected = "Minimum"),
        fluidRow(column(6,numericInput("freq_roll_days", label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
                 column(6,selectInput("freq_roll_align", label = "Alignment:",
                                      choices = list("Right" = "right", "Left" = "left", "Center" = "center"), selected = "Right"))),
        checkboxInput("freq_ign_missing_box", "Calculate statistics despite missing values", value = FALSE)
    ),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot",
        # selectInput("freq_usemax", label = "Peak flow data:", choices = list("Minimum" = FALSE,"Maximum" = TRUE), selected = "Minimum"),
        checkboxInput("freq_usemax", label = "Use maximum data data", value = FALSE),
        checkboxInput("freq_uselog", label = "Use log-transformed data", value = FALSE),
        selectInput("freq_prob_plot_position", label = "Probability plot position", choices = c("weibull", "median","hazen"), selected = "weibull"),
        selectInput("freq_fit_distr", label = "fit_distr", choices = c("PIII", "weibull"), selected = "PIII"),
        selectInput("freq_fit_distr_method", label = "fit_distr_method (add ifelse)", choices = list("method of moments" = "MOM",
                                                                                                     "maximum likelihood estimate" = "MLE"), selected = "MOM"),
        plotlyOutput("freq_freqplot"),
        textOutput("freq_fit"),
        dataTableOutput("freq_Q_stat"),
        dataTableOutput("freq_plotdata"),
        dataTableOutput("freq_fitted_quantiles")

      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "R Code",
        h4("Copy and paste the following into an R console or script to reproduce the results."),

        htmlOutput("freq_code")
      ) # end of tabPanel
    ) # end of tabsetPanel
  ) # end of mainPanel
)



# #### Map ####
#

# tabPanel(
#   title = "HYDAT Stations",
#   h5("Put nice map here (that can be filtered by the table below?) :)"),
#   DT::dataTableOutput("hydat_stations_table")
# )# end of tapPanel


# Combine -------------------------------------------------------------------
dashboardPage(skin = "green",
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
               menuSubItem("Single stats", tabName = "sum_single")),
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
               menuSubItem("Flow Frequency", tabName = "comp_flow"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home", ui_home),
      tabItem("settings", ui_settings),
      tabItem("data_load", ui_data_load),
      tabItem("data_screen", ui_data_screen),
      tabItem("sum_general", ui_sum_general),
      tabItem("sum_flow", ui_sum_flow),
      tabItem("sum_single", ui_sum_single),
      tabItem("cumulative", ui_cumulative),
      tabItem("ah_flow_timing", ui_ah_flow_timing),
      tabItem("ah_low_flows", ui_ah_low_flows),
      tabItem("ah_peak", ui_ah_peak),
      tabItem("ah_outside_normal", ui_ah_outside_normal),
      tabItem("comp_annual", ui_comp_annual),
      tabItem("comp_flow", ui_comp_flow)
    )
  )
)

