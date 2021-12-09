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

ui <- navbarPage(
  title = "fasstr shiny",
  theme = shinytheme('flatly'),

  #### Data Selection #####

  tabPanel(
    title = "Data",
    sidebarPanel(
      width = 4,
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
    mainPanel(
      tabsetPanel(
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
    )),


  ##### Data Screening #####

  tabPanel(
    title = "Screening",
    sidebarPanel(width = 3,
                 h4("Data Overview")
    ),
    mainPanel(
      tabsetPanel(
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
  ),

  ##### Long-term Flows #####

  navbarMenu(
    title = "Long-term",
    tabPanel(
      title = "Summary Statistics",
      sidebarPanel(
        width = 3,
        h4("Long-term Statistics"),
        helpText("Calculates the long-term and monthly-long term statistics."),
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
      mainPanel(
        tabsetPanel(
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
      )),
    tabPanel(
      title = "Flow Duration and Percentiles",
      sidebarPanel(
        width = 3,
        h4("Percentiles and Flow Duration"),
        selectInput("ptile_datatype", label = "Discharge type:", choices = list("Discharge (cms)" = 1,
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
      mainPanel(
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
    ),
    tabPanel(
      title = "Single Statistics",
      sidebarPanel(
        h4("long-term MAD"),
        h4("long-term percentile"),
        h4("long-term percentile rank")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot"
          ),
          tabPanel("Table"
          ),
          tabPanel("Info"
          )
        )
      )
    )),

  ##### Annual Flows #####

  navbarMenu(
    title = "Annual",
    tabPanel(
      title = "Summary Statistics",
      sidebarPanel(
        width = 3,
        # h4("Summary Statistics"),
        selectInput("ann_datatype", label = "Discharge type:", choices = list("Discharge (cms)" = 1,
                                                                              "Volumetric Discharge (m3)" = 2,
                                                                              "Runoff Yield (mm)" = 3),
                    selected = 1),
        fluidRow(column(6,numericInput("ann_roll_days", label = "Rolling average days:", value = 1, min = 1, max = 180, step = 1)),
                 column(6,selectInput("ann_roll_align", label = "Rolling alignment:",
                                      choices = list("Right" = "right", "Left" = "left", "Center" = "center"), selected = "Right"))),
        selectizeInput("annual_months",
                       label = "Months:",
                       choices = list("Jan" = 1, "Feb" = 2,
                                      "Mar" = 3, "Apr" = 4,
                                      "May" = 5, "Jun" = 6,
                                      "Jul" = 7, "Aug" = 8,
                                      "Sep" = 9, "Oct" = 10,
                                      "Nov" = 11, "Dec" = 12),
                       selected = c(1:12),
                       multiple = TRUE),
        selectizeInput("ann_ptiles",
                       label = "Percentiles to calculate:",
                       choices = c(1:99),
                       selected = c(10,90),
                       multiple = TRUE),
        checkboxInput("ign_missing_box", "Calculate statistics despite missing values", value = FALSE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Plot", #br(),
            plotlyOutput('annual_plot'),
            fluidRow(
              column(width = 6,
                     h4("Plotting Options"),
                     uiOutput("annual_params"),
                     checkboxInput("ann_logQ", "Plot discharge on log scale", value = FALSE),
                     textInput("ann_plot_title", label = "Plot title:", value = NULL)),
              column(width = 6,
                     h4("Downloading"),
                     selectInput("ann_plottype", "File type:", choices = c("png", "jpeg", "pdf", "bmp"), selected = "png"),
                     downloadButton('download_annual_plot', 'Download Plot')))
          ),
          tabPanel(
            title = "Table",
            selectInput("ann_filetype", "File type:", choices = c("csv", "xls", "xlsx"), selected = "csv"),
            downloadButton('download_annual_table', 'Download Table'),
            DT::dataTableOutput("annual_table")
          ),
          tabPanel(
            title = "R-Code"
          )
        )
      )),
    tabPanel(
      title = "Low Flows",
      sidebarPanel(
        h4("Low Flows")
      ),
      mainPanel(
        tabsetPanel(
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
      )),
    tabPanel(
      title = "Flow Timing",
      sidebarPanel(
        h4("Flow Timing")
      ),
      mainPanel(
        tabsetPanel(
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
      )),
    tabPanel(
      title = "Days Outside Normal",
      sidebarPanel(
        h4("TITLE")
      ),
      mainPanel(
        tabsetPanel(
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
    ),
    tabPanel(
      title = "Cumulative?",
      sidebarPanel(
        h4("group by day or group by month; or have in other tabs")
      ),
      mainPanel(
        tabsetPanel(
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
    )),

  ##### Monthly Flows #####

  navbarMenu(
    title = "Monthly",
    tabPanel(
      title = "Summary Statistics",
      sidebarPanel(
        h4("Monthly Statistics")
      ),
      mainPanel(
        tabsetPanel(
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
      )),
    tabPanel(
      title = "Cumulative Statistics",
      sidebarLayout(
        sidebarPanel(
          h4("Monthly Statistics")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Plot"
            ),
            tabPanel(
              title = "Table"
            ),
            tabPanel(
              title = "Info"
            )
          ))
      ))),

  ##### Daily Flows #####

  navbarMenu(
    title = "Daily",
    tabPanel(
      title = "Summary Statistics",
      sidebarPanel(width = 3,
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
      mainPanel(
        tabsetPanel(
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
    tabPanel(
      title = "Cumulative Statistics",
      sidebarLayout(
        sidebarPanel(
          h4("Daily Cumulative (or in own section??")
        ),
        mainPanel(
          tabsetPanel(
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
        )))),

  ##### Annual Trends #####

  tabPanel(
    title = "Annual Trends",
    sidebarPanel(width = 3,
                 h4("Annual Trending"),
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
    mainPanel(
      tabsetPanel(
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
    )),

  ##### Flow Frequencty #####

  tabPanel(
    title = "Flow Frequency",
    sidebarPanel(
      width = 3,
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
    mainPanel(
      tabsetPanel(
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
  ),


  #### Map ####


  tabPanel(
    title = "HYDAT Stations",
    h5("Put nice map here (that can be filtered by the table below?) :)"),
    DT::dataTableOutput("hydat_stations_table")
  )# end of tapPanel
)
