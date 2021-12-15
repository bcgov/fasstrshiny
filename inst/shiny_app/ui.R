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
  column(width = 12,
         build_ui(id = "opts", define_options = TRUE,
                  include = c("discharge", "missing", "rolling", "months",
                              "percentiles"))
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
        bsButton("data_hydat_map_select", "Use selected station", style = "primary"),
        leafletOutput("data_hydat_map", width = "100%", height = "500px")
      ),

      ### HYDAT Table --------
      tabPanel(
        title = "HYDAT Table", width = 12,
        bsButton("data_hydat_table_select", "Use selected station", style = "primary"),
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


# Stats Summary ----------------
ui_summary <- fluidRow(
  column(
    width = 12, h2("Summary Statistics"),
    box(
      width = 3,
      radioButtons("summary_type", label = "Summary type",
                   choices = list("Long-term", "Annual", "Monthly", "Daily")),
      uiOutput("ui_sum"),
    ),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot",
        plotOutput('lt_plot'),
        h4("Plotting Options"),
        uiOutput("lt_params"),
        checkboxInput("lt_logQ", "Plot discharge on log scale", value = FALSE),
        textInput("lt_plot_title", label = "Plot title:", value = NULL), br(),
        downloadButton('download_lt_plot', 'Download Plot')
      ),
      tabPanel(
        title = "Table", br(),
        downloadButton('download_lt_table', 'Download Table'),
        dataTableOutput("lt_table")
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)


### Annual Summary Stats --------------------------
# tabPanel(
#   title = "Summary Statistics",
#
#   tabBox(
#     width = 9,
#     tabPanel(
#       title = "Plot", #br(),
#       plotlyOutput('annual_plot'),
#       fluidRow(
#         column(width = 6,
#                h4("Plotting Options"),
#                uiOutput("annual_params"),
#                checkboxInput("ann_logQ", "Plot discharge on log scale", value = FALSE),
#                textInput("ann_plot_title", label = "Plot title:", value = NULL)),
#         column(width = 6,
#                h4("Downloading"),
#                selectInput("ann_plottype", "File type:", choices = c("png", "jpeg", "pdf", "bmp"), selected = "png"),
#                downloadButton('download_annual_plot', 'Download Plot')))
#     ),
#     tabPanel(
#       title = "Table",
#       selectInput("ann_filetype", "File type:", choices = c("csv", "xls", "xlsx"), selected = "csv"),
#       downloadButton('download_annual_table', 'Download Table'),
#       DT::dataTableOutput("annual_table")
#     ),
#     tabPanel(
#       title = "R-Code"
#     )
#   )
# )


## Flow duration and Percentiles --------------
ui_sum_flow <- fluidRow(
  column(
    width = 12, h2("Flow duration and Percentiles"),
    box(
      width = 3,
      uiOutput("ui_sum_flow")
    ),
    column(width = 9,
           tabsetPanel(
             tabPanel(
               title = "Plot",
               plotOutput('ptile_plot'),
               h4("Plotting Options"),
               uiOutput("ptile_params"),
               checkboxInput("ptile_logQ", "Plot discharge on log scale", value = FALSE),
               textInput("ptile_plot_title", label = "Plot title:", value = NULL), br(),
               downloadButton('download_ptile_plot', 'Download Plot')
             ),
             tabPanel(
               title = "Table", br(),
               downloadButton('download_ptile_table', 'Download Table'),
               dataTableOutput("ptile_table")
             ),
             tabPanel(
               title = "Info"
             )
           )
    )
  )
)



## Single stats --------------
ui_sum_single <- fluidRow(
  column(
    width = 12, h2("Single Statistics"),
    box(
      width = 3,

      h4("long-term Mean Annual Discharge"),
      h4("long-term percentile"),
      h4("long-term percentile rank")
    ),
    tabBox(
      width = 9, height = min_height,
      tabPanel("Plot"
      ),
      tabPanel("Table"
      ),
      tabPanel("Info"
      )
    )
  )
)



# Annual Hydrograph ------------------------------

## Flows low ------------------------
ui_annual_flow_low <- fluidRow(
  column(
    width = 12, h2("Low flows"),
    box(width = 3),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)

## Flow timing ------------------------
ui_annual_flow_timing <- fluidRow(
  column(
    width = 12, h2("Flow timing"),
    box(width = 3),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)

## Flow peak ------------------------
ui_annual_flow_peak <- fluidRow(
  column(
    width = 12, h2("Flow peak"),
    box(width = 3),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)

## Days outside normal ------------------------
ui_annual_outside_normal <- fluidRow(
  column(
    width = 12, h2("Days outside normal"),
    box(width = 3),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)


## Daily ------------------------
ui_sum_daily <- fluidRow(
  column(
    width = 12,
    h2("Daily"),
    tabBox(
      width = 12, height = min_height,

      ### Summary Stats -------------------------
      tabPanel(
        title = "Summary Statistics",
        column(width = 3,
               h4("Daily Flows"),
               selectInput("dly_datatype", label = "Discharge type:", choices = list("Discharge (cms)" = 1,
                                                                                     "Volumetric Discharge (m3)" = 2,
                                                                                     "Runoff Yield (mm)" = 3),
                           selected = 1),
               fluidRow(column(6,numericInput("dly_roll_days", label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
                        column(6,selectInput("dly_roll_align", label = "Rolling alignment:",
                                             choices = list("Right" = "right", "Left" = "left", "Center" = "center"), selected = "Right"))),
               selectizeInput("dly_months",
                              label = "Months:",
                              choices = list("Jan" = 1, "Feb" = 2,
                                             "Mar" = 3, "Apr" = 4,
                                             "May" = 5, "Jun" = 6,
                                             "Jul" = 7, "Aug" = 8,
                                             "Sep" = 9, "Oct" = 10,
                                             "Nov" = 11, "Dec" = 12),
                              selected = c(1:12),
                              multiple = TRUE),
               selectizeInput("dly_ptiles",
                              label = "Percentiles to calculate:",
                              choices = c(1:99),
                              selected = c(10,90),
                              multiple = TRUE),
               checkboxInput("dly_ign_missing_box", "Calculate statistics despite missing values", value = FALSE),
               h5("Display Settings"),
               checkboxInput("logDaily", label = "Plot Discharge axis on log scale", value= TRUE),
               checkboxInput("yearCheckDaily", label = "Plot daily discharge from a selected year (below)", value= FALSE),
               uiOutput("yearSelectDaily")
        ),
        column(width = 9,
               tabBox(height = min_height,
                 tabPanel(
                   title = "Plot",br(),
                   plotOutput('dailyPlot'),br(),
                   downloadButton('downloadDailyPlot', 'Download Plot')
                 ),
                 tabPanel(
                   title = "Data",
                   br(),
                   downloadButton('downloadDailyTable', 'Download Table'),
                   br(),br(),
                   dataTableOutput("dailyTable")),
                 tabPanel(
                   title = "Info")
               )
        )),

    ))
)

# Stats Cumulative -------------

## Long Term ---------------------------------

ui_cum_lt <- fluidRow(

  column(
    width = 12, h2("Long Term Cumulative Statistics"),
    box(width = 3,
        h4("group by day or group by month; or have in other tabs")
    ),
    tabBox(width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)

## Monthly -------------------------
ui_cum_monthly <-  fluidRow(
  column(
    width = 12, h2("Monthly Cumulative Statistics"),
    box(width = 3),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
      )
    )
  )
)

## Daily -------------------
ui_cum_daily <- fluidRow(
  column(
    width = 12, h2("Daily Cumulative Statistics"),
    box(width = 3),
    tabBox(
      width = 9, height = min_height,
      tabPanel(
        title = "Plot"
      ),
      tabPanel(
        title = "Table"
      ),
      tabPanel(
        title = "Info"
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
        br(),
        fluidRow(column(3, DT::dataTableOutput("trends_results_data")),
                 column(9, plotlyOutput("trends_plot")))
      ),
      tabPanel(
        title = "R Code",
        h4("Copy and paste the following into an R console or script to reproduce the results."),
        br(),
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
        br(),
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
    sidebarMenu(
      id = "menu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Data", tabName = "data", icon = icon("table"),
               menuSubItem("Loading", tabName = "data_load"),
               menuSubItem("Screening", tabName = "data_screen")),
      menuItem("Summary statistics", tabName = "summary",
               icon=icon("chart-bar"),
               menuSubItem("General", tabName = "summary"),
               menuSubItem("Flow duration and percentiles",
                           tabName = "sum_flow"),
               menuSubItem("Single stats", tabName = "sum_single")),
      menuItem("Cumulative Stats", tabName = "cumulative",
               icon = icon("chart-area"),
               menuSubItem("Long-term", tabName = "cum_lt"),
               menuSubItem("Annual", tabName = "cum_annual"),
               menuSubItem("Monthly", tabName = "cum_monthly"),
               menuSubItem("Daily", tabName = "cum_daily")),
      menuItem("Annual Hydrograph Stats", tabName = "annual",
               icon = icon("calendar"),
               menuSubItem("Low Flows", tabName = "annual_flow_low"),
               menuSubItem("Flow timing", tabName = "annual_flow_timing"),
               menuSubItem("Peak Flows", tabName = "annual_flow_peak"),
               menuSubItem("Days outside normal",
                           tabName = "annual_outside_normal")),
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
      tabItem("summary", ui_summary),
      tabItem("sum_flow", ui_sum_flow),
      tabItem("sum_single", ui_sum_single),
      ##tabItem("sum_annual", ui_sum_annual),
      #tabItem("sum_monthly", ui_sum_monthly),
      #tabItem("sum_daily", ui_sum_daily),
      tabItem("cum_lt", ui_cum_lt),
      tabItem("cum_monthly", ui_cum_monthly),
      tabItem("cum_daily", ui_cum_daily),
      tabItem("annual_flow_low", ui_annual_flow_low),
      tabItem("annual_flow_timing", ui_annual_flow_timing),
      tabItem("annual_flow_peak", ui_annual_flow_peak),
      tabItem("annual_outside_normal", ui_annual_outside_normal),
      tabItem("comp_annual", ui_comp_annual),
      tabItem("comp_flow", ui_comp_flow)
    )
  )
)

