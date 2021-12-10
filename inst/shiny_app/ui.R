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
  column(width = 12)
)

# Data -------------------------

## Data Loading ------------------
ui_data_load <- fluidRow(
  column(width = 12, h2("Loading Data"),
         box(width = 4,
             helpText("Data Source:"),
             radioGroupButtons(inputId = "data_source",
                               label = NULL, choices = c("HYDAT", "CSV File"),
                               selected = "HYDAT"),
             conditionalPanel("input.data_source == 'HYDAT'",
                              uiOutput("station_num")),
             conditionalPanel("input.data_source != 'HYDAT'",
                              fileInput('file1', label = NULL,
                                        accept=c('text/csv',
                                                 'text/comma-separated-values,text/plain',
                                                 '.csv'))),
             actionButton("data_select", "Select"),
             hr(),
             helpText("Station Information:"),
             fluidRow(column(width = 6, uiOutput("station_name")),
                      column(width = 6, uiOutput("basinarea"))),
             helpText("Dates Filtering:"),
             #helpText("Review the data and select the years of interest to be analyzed."),
             selectInput("year_start",
                         label = "Select a year period:",
                         choices = list("Jan-Dec" = 1, "Feb-Jan" = 2,
                                        "Mar-Feb" = 3, "Apr-Mar" = 4,
                                        "May-Apr" = 5, "Jun-May" = 6,
                                        "Jul-Jun" = 7, "Aug-Jul" = 8,
                                        "Sep-Aug" = 9, "Oct-Sep" = 10,
                                        "Nov-Oct" = 11, "Dec-Nov" = 12),
                         selected = 1),
             #h4("Analysis Options"),
             #  selectInput("yearType",label = "Select a year type:", c("Calendar Year (Jan-Dec)"=1,"Water Year (Oct-Sep)"=2)),
             # conditionalPanel("input.data_select%2>0",
             uiOutput("years_range"),
             uiOutput("years_exclude"),
             helpText("Session Settings:"),
             fluidRow(actionButton("save_session","Save Settings"),
                      actionButton("load_session","Load Settings"))
             #)
         ),
         box(width = 8,
             tabBox(
               tabPanel(
                 title = "Plot",
                 plotlyOutput('timeseries_plot'),
                 fluidRow(column(4, br(),uiOutput("dateRange")),
                          column(5, br(),checkboxInput("logTimeSeries", label = "Plot Discharge axis on log scale", value= FALSE)),
                          column(2, br(),downloadButton('downloadtimeseries_plot', 'Download Plot'))),
                 selectInput("data_datatype", label = "Discharge type:",
                             choices = list("Discharge (cms)" = 1,
                                            "Volumetric Discharge (m3)" = 2,
                                            "Runoff Yield (mm)" = 3))
               ),
               tabPanel(
                 title = "Table", br(),
                 DT::dataTableOutput("timeseries_data")
               ),
               tabPanel(
                 title = "R Code")#data
             )
         )
  )
)

## Data Screening ---------------------
ui_data_screen <- fluidRow(
  column(width = 12, h2("Data Screening"),
         tabBox(
           tabPanel(
             title = "Data Summary Plot",
             fluidRow(column(9,br(),selectInput("summaryY", "Select annual daily metric to review:",
                                                c("Mean","Maximum","Minimum","StandardDeviation"))),
                      column(2,br(),downloadButton('downloadSummaryPlot','Download Plot'))),
             plotlyOutput('summaryPlot')
           ),
           tabPanel(
             title = "Data Availability Plot", br(),
             selectizeInput("availability_months",
                            label = "Months:",
                            choices = list("Jan" = 1, "Feb" = 2,
                                           "Mar" = 3, "Apr" = 4,
                                           "May" = 5, "Jun" = 6,
                                           "Jul" = 7, "Aug" = 8,
                                           "Sep" = 9, "Oct" = 10,
                                           "Nov" = 11, "Dec" = 12),
                            selected = c(1:12),
                            multiple = TRUE),
             downloadButton('download_missing_plot', 'Download Plot'),br(),
             plotlyOutput("missing_plot")
           ),
           tabPanel(
             title = "Table", br(),
             downloadButton('download_summary_table', 'Download Table'), br(),
             DT::dataTableOutput("summary_table")
           ),
           tabPanel(
             title = "R Code"
           )
         )
  )
)


# Stats Summary ----------------
ui_stats_summary <- fluidRow(
  column(
    width = 12, h2("Summary Statistics"),
    box(
      width = 3,
      selectInput("summary_type", label = "Summary type:",
                  choices = list("Lont-term", "Annual", "Monthly", "Daily")),
      selectInput("lt_datatype", label = "Discharge type:",
                  choices = list("Discharge (cms)" = 1,
                                 "Volumetric Discharge (m3)" = 2,
                                 "Runoff Yield (mm)" = 3),
                  selected = 1),
      fluidRow(column(6, numericInput("lt_roll_days", label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
               column(6, selectInput("lt_roll_align", label = "Rolling alignment:",
                                     choices = list("Right" = "right", "Left" = "left", "Center" = "center"), selected = "Right"))),
      selectizeInput("lt_months",
                     label = "Custom Months:",
                     choices = list("Jan" = 1, "Feb" = 2,
                                    "Mar" = 3, "Apr" = 4,
                                    "May" = 5, "Jun" = 6,
                                    "Jul" = 7, "Aug" = 8,
                                    "Sep" = 9, "Oct" = 10,
                                    "Nov" = 11, "Dec" = 12),
                     selected = NULL,
                     multiple = TRUE),
      textInput('lt_months_label', label = "Custom Months Label:", placeholder = "ex. Jun-Aug"),
      selectizeInput("lt_ptiles",
                     label = "Percentiles to calculate:",
                     choices = c(1:99),
                     selected = c(10,90),
                     multiple = TRUE),
      checkboxInput("lt_ign_missing_box", "Calculate statistics despite missing values", value = FALSE)
    ),
    tabBox(
      width = 9,
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
#   box(
#     width = 3,
#     selectInput("ann_datatype", label = "Discharge type:",
#                 choices = list("Discharge (cms)" = 1,
#                                "Volumetric Discharge (m3)" = 2,
#                                "Runoff Yield (mm)" = 3),
#                 selected = 1),
#     fluidRow(
#       column(6,numericInput("ann_roll_days",
#                             label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
#       column(6,selectInput("ann_roll_align", label = "Rolling alignment:",
#                            choices = list("Right" = "right", "Left" = "left", "Center" = "center"), selected = "Right"))),
#     selectizeInput("annual_months",
#                    label = "Months:",
#                    choices = list("Jan" = 1, "Feb" = 2,
#                                   "Mar" = 3, "Apr" = 4,
#                                   "May" = 5, "Jun" = 6,
#                                   "Jul" = 7, "Aug" = 8,
#                                   "Sep" = 9, "Oct" = 10,
#                                   "Nov" = 11, "Dec" = 12),
#                    selected = c(1:12),
#                    multiple = TRUE),
#     selectizeInput("ann_ptiles",
#                    label = "Percentiles to calculate:",
#                    choices = c(1:99),
#                    selected = c(10,90),
#                    multiple = TRUE),
#     checkboxInput("ign_missing_box", "Calculate statistics despite missing values", value = FALSE)
#   ),
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
ui_stats_sum_flow <- fluidRow(
  column(
    width = 12, h2("Flor duration and Percentiles"),
    box(
      width = 3,
      selectInput("ptile_datatype", label = "Discharge type:",
                  choices = list("Discharge (cms)" = 1,
                                 "Volumetric Discharge (m3)" = 2,
                                 "Runoff Yield (mm)" = 3),
                  selected = 1),
      fluidRow(column(6,numericInput("ptile_roll_days", label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
               column(6,selectInput("ptile_roll_align", label = "Rolling alignment:",
                                    choices = list("Right" = "right", "Left" = "left", "Center" = "center"), selected = "Right"))),
      selectizeInput("ptile_months_cust",
                     label = "Custom Months:",
                     choices = list("Jan" = 1, "Feb" = 2,
                                    "Mar" = 3, "Apr" = 4,
                                    "May" = 5, "Jun" = 6,
                                    "Jul" = 7, "Aug" = 8,
                                    "Sep" = 9, "Oct" = 10,
                                    "Nov" = 11, "Dec" = 12),
                     selected = NULL,
                     multiple = TRUE),
      textInput('ptile_months_cust_label', label = "Custom Months Label:", placeholder = "ex. Jun-Aug"),
      checkboxInput("ptile_ign_missing_box", "Calculate statistics despite missing values", value = FALSE)
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
ui_stats_sum_single <- fluidRow(
  column(
    width = 12, h2("Single Statistics"),
    box(
      width = 3,

      h4("long-term Mean Annual Discharge"),
      h4("long-term percentile"),
      h4("long-term percentile rank")
    ),
    tabBox(
      width = 9,
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
ui_stats_annual_flow_low <- fluidRow(
  column(
    width = 12, h2("Low flows"),
    box(width = 3),
    tabBox(
      width = 9,
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
ui_stats_annual_flow_timing <- fluidRow(
  column(
    width = 12, h2("Flow timing"),
    box(width = 3),
    tabBox(
      width = 9,
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
ui_stats_annual_flow_peak <- fluidRow(
  column(
    width = 12, h2("Flow peak"),
    box(width = 3),
    tabBox(
      width = 9,
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
ui_stats_annual_outside_normal <- fluidRow(
  column(
    width = 12, h2("Days outside normal"),
    box(width = 3),
    tabBox(
      width = 9,
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

## Monthly ----------------------------

ui_stats_sum_monthly <- fluidRow(
  column(
    width = 12,
    h2("Monthly"),
    tabBox(

      ### Summary Stats -------------------------
      tabPanel(
        title = "Summary Statistics", width = 12,
        tabBox(width = 12,
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

  )
)


## Daily ------------------------
ui_stats_sum_daily <- fluidRow(
  column(
    width = 12,
    h2("Daily"),
    tabBox(

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
               tabBox(
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

ui_stats_cum_lt <- fluidRow(

  column(
    width = 12, h2("Long Term Cumulative Statistics"),
    box(width = 3,
        h4("group by day or group by month; or have in other tabs")
    ),
    tabBox(width = 9,
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
ui_stats_cum_monthly <-  fluidRow(
  column(
    width = 12, h2("Monthly Cumulative Statistics"),
    box(width = 3),
    tabBox(
      width = 9,
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
ui_stats_cum_daily <- fluidRow(
  column(
    width = 12, h2("Daily Cumulative Statistics"),
    box(width = 3),
    tabBox(
      width = 9,
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
ui_stats_comp_annual <- fluidRow(
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
      width = 9,
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

ui_stats_comp_flow <- fluidRow(
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
      width = 9,
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
      menuItem("Summary statistics", tabName = "stats_summary",
               icon=icon("chart-bar"),
               menuSubItem("General", tabName = "stats_summary"),
               menuSubItem("Flow duration and percentiles",
                           tabName = "stats_sum_flow"),
               menuSubItem("Single stats", tabName = "stats_sum_single")),
      menuItem("Cumulative Stats", tabName = "stats_cumulative",
               icon = icon("chart-area"),
               menuSubItem("Long-term", tabName = "stats_cum_lt"),
               menuSubItem("Annual", tabName = "stats_cum_annual"),
               menuSubItem("Monthly", tabName = "stats_cum_monthly"),
               menuSubItem("Daily", tabName = "stats_cum_daily")),
      menuItem("Annual Hydrograph Stats", tabName = "stats_annual",
               icon = icon("calendar"),
               menuSubItem("Low Flows", tabName = "stats_annual_flow_low"),
               menuSubItem("Flow timing", tabName = "stats_annual_flow_timing"),
               menuSubItem("Peak Flows", tabName = "stats_annual_flow_peak"),
               menuSubItem("Days outside normal",
                           tabName = "stats_annual_outside_normal")),
      menuItem("Computations", tabName = "stats_computed",
               icon = icon("chart-line"),
               menuSubItem("Annual Trends", tabName = "stats_comp_annual"),
               menuSubItem("Flow Frequency", tabName = "stats_comp_flow"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home", ui_home),
      tabItem("settings", ui_settings),
      tabItem("data_load", ui_data_load),
      tabItem("data_screen", ui_data_screen),
      tabItem("stats_summary", ui_stats_summary),
      tabItem("stats_sum_flow", ui_stats_sum_flow),
      tabItem("stats_sum_single", ui_stats_sum_single),
      ##tabItem("stats_sum_annual", ui_stats_sum_annual),
      #tabItem("stats_sum_monthly", ui_stats_sum_monthly),
      #tabItem("stats_sum_daily", ui_stats_sum_daily),
      tabItem("stats_cum_lt", ui_stats_cum_lt),
      tabItem("stats_cum_monthly", ui_stats_cum_monthly),
      tabItem("stats_cum_daily", ui_stats_cum_daily),
      tabItem("stats_annual_flow_low", ui_stats_annual_flow_low),
      tabItem("stats_annual_flow_timing", ui_stats_annual_flow_timing),
      tabItem("stats_annual_flow_peak", ui_stats_annual_flow_peak),
      tabItem("stats_annual_outside_normal", ui_stats_annual_outside_normal),
      tabItem("stats_comp_annual", ui_stats_comp_annual),
      tabItem("stats_comp_flow", ui_stats_comp_flow)
    )
  )
)

