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
      conditionalPanel(
        "input.data_show_stn == true",
        fluidRow(
          column(
            width = 6,
            textInput('data_station_name',
                      label = "Name",
                      placeholder = "ex. Mission Creek")),
          column(
            width = 6,
            numericInput("data_basin_area",
                         label = html("Basin area (km<sup>2</sup>)"), value = 0,
                         min = 0, step = 0.1)))),

      show("data_show_dates", "Dates"),
      conditionalPanel("input.data_show_dates == true",
                       uiOutput("ui_data_water_year"),
                       uiOutput("ui_data_years_range"),
                       uiOutput("ui_data_years_exclude"),
                       uiOutput("ui_data_months")),

      show("data_show_types", "Data types"),
      conditionalPanel("input.data_show_types == true",
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
                           "Months to include/exclude from the plot"),
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


# Stats Summary ---------------------------------------------------------------

## General ----------------
ui_sum_general <- fluidRow(
  column(
    width = 12, h2("General Summary Statistics"),
    box(
      width = 3,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      radioGroupButtons("sum_type",
                        label = "Summary type", size = "sm",
                        choices = list("Long-term",
                                       "Annual",
                                       "Monthly", "Daily"),
                        selected = "Long-term",
                        status = "primary", justified = TRUE),
      bsTooltip("sum_type", "Type of summary statistics to calculate"),
      selectizeInput("sum_mad",
                  label = "Mean Annual Discharge percentiles",
                  choices = c(1:99),
                  selected = c(1, 5, 50, 95, 99),
                  multiple = TRUE),
      bsTooltip("sum_mad", tips$mad),
      uiOutput("ui_sum"),
      uiOutput("ui_sum_miss_allowed")
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_sum_monthly_plot"), #Only when monthly
        uiOutput("ui_sum_plot_options", align = "right"),
        girafeOutput("sum_plot", height = "450px")
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
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      numericInput("sumfl_flow",
                   label = "Flow value for percentile",
                   value = 10, min = 0),
      bsTooltip("sumfl_flow", tips$flow),
      uiOutput("ui_sumfl"),
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot - Flow duration",
        uiOutput("ui_sumfl_plot_options", align = "right"),
        girafeOutput("sumfl_plot", height = "400px"),
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
      #helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        uiOutput("ui_sumam"),
        girafeOutput("sumam_plot", height = "400px")
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
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      radioGroupButtons("cum_type",
                        label = "Cumulative type",
                        choices = list("Annual", "Monthly", "Daily"),
                        justified = TRUE, status = "primary"),
      bsTooltip("cum_type", "Type of cumulative statistics to calculate"),
      radioButtons("cum_discharge",
                   label = "Discharge type",
                   choices = list("Volumetric Discharge (m3)" = FALSE,
                                  "Runoff Yield (mm)" = TRUE)),
      bsTooltip("cum_discharge", tips$discharge),
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


# Annual Hydrograph Stats -----------------------------------------------------

## Flow timing ------------------------------
ui_ah_flow_timing <- fluidRow(
  column(
    width = 12, h2("Flow Timing"),
    box(
      width = 12,
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      div(style = "max-width: 300px;",
          selectizeInput("ahft_percent",
                      label = "Percents of total annual flows",
                      choices = c(1:99),
                      selected = c(25, 33, 50, 75),
                      multiple = TRUE),
          bsTooltip("ahft_percent", tips$percent)
      ),

      tabBox(
        width = 12, height = min_height,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          girafeOutput("ahft_plot", height = "400px")
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
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      select_rolling("ahlf", set = FALSE, multiple = TRUE),
      uiOutput("ui_ahlf")
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        girafeOutput("ahlf_plot", height = "450px")
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
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      uiOutput("ui_ahp"),
      select_rolling("ahp", set = FALSE)
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
      helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
      sliderInput("ahon_normal", label = "Normal range ",
                  value = c(25, 75), min = 1, max = 99, step = 1),
      bsTooltip("ahon_normal", tips$normal)
    ),
    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        girafeOutput("ahon_plot", height = "500px")
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
        bsTooltip("at_zyp", tips$zyp),
        bsTooltip("at_alpha", tips$alpha),

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
        bsTooltip("at_annual_percentiles", tips$percentiles),
        bsTooltip("at_monthly_percentiles", tips$percentiles),

        strong("Low Flows"),
        select_rolling("at_low", set = FALSE, multiple = TRUE),

        selectizeInput("at_percent",
                       label = "Percents of total annual flows",
                       choices = c(1:99),
                       selected = c(25, 33, 50, 75),
                       multiple = TRUE),
        bsTooltip("at_percent", tips$percent),

        sliderInput("at_normal", label = "Days Outside Normal - Range",
                    value = c(25, 75), min = 1, max = 99, step = 1),
        bsTooltip("at_normal", tips$normal),

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
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        hr(class = "narrowHr"),

        # Other
        uiOutput("ui_vf_exclude"),
        select_rolling("vf", set = FALSE, multiple = TRUE),
        uiOutput("ui_vf"),

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
        bsTooltip("vf_use_max", tips$use_max),
        bsTooltip("vf_fit_distr", tips$fit_distr),

        selectizeInput(
          "vf_fit_quantiles",
          label = "Quantiles to estimate",
          choices = seq(0.01, 0.999, 0.0025),
          selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                       0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
          multiple = TRUE),
        bsTooltip("vf_fit_quantiles", tips$fit_quantiles),

        fluidRow(
          column(
            width = 6,
            materialSwitch("vf_plot_curve", label = "Plot curve", value = TRUE,
                           status = "success")),
          column(
            width = 6,
            materialSwitch("vf_use_log",
                           label = "Log trans",
                           value = FALSE, status = "success"))
        ),
        bsTooltip("vf_plot_curve", tips$plot_curve),
        bsTooltip("vf_use_log", tips$use_log),

        fluidRow(
          column(6, awesomeRadio("vf_prob_plot",
                                 label = "Plotting positions",
                                 choices = list("Weibull" = "weibull",
                                                "Median" = "median",
                                                "Hazen" = "hazen"))),
          column(6, textInput(
            "vf_prob_scale",
            label = "Probabilies to plot",
            value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001"))
        ),
        bsTooltip("vf_prob_plot", tips$prob_plot),
        bsTooltip("vf_prob_scale", tips$prob_scale)
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
        bsTooltip("hp_use_max", tips$use_max),
        bsTooltip("hp_fit_distr", tips$fit_distr),

        selectizeInput(
          "hp_fit_quantiles",
          label = "Quantiles to estimate",
          choices = seq(0.01, 0.999, 0.0025),
          selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                       0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
          multiple = TRUE),
        bsTooltip("hp_fit_quantiles", tips$fit_quantiles),

        fluidRow(
          column(width = 6,
                 materialSwitch("hp_plot_curve", label = "Plot curve", value = TRUE,
                                status = "success")),
          column(width = 6,
                 materialSwitch("hp_use_log",
                                label = "Log trans",
                                value = FALSE, status = "success"))
        ),
        bsTooltip("hp_plot_curve", tips$plot_curve),
        bsTooltip("hp_use_log", tips$use_log),

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
        bsTooltip("hp_prob_plot", tips$prob_plot),
        bsTooltip("hp_prob_scale", tips$prob_scale),
    ),

    tabBox(
      width = 9, height = min_height,

      ### Plot ---------------------
      tabPanel(
        title = "Plot",
        girafeOutput("hp_plot")
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
        menuItem("Data", tabName = "data", icon = icon("table"),
                 menuSubItem("Loading", tabName = "data_load"),
                 menuSubItem("Availability", tabName = "data_available")),
        menuItem("Overview", tabName = "overview", icon = icon("binoculars")),
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
        tabItem("data_load", ui_data_load),
        tabItem("data_available", ui_data_available),
        tabItem("overview", ui_overview),
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

