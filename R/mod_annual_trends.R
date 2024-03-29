# Copyright 2022 Province of British Columbia
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

ui_annual_trends <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Trends"),
      box(width = 3,
          helpText("Explore trending of various annual metrics using prewhitened, ",
                   "non-parametric Mann-Kendall tests and slopes estimated using ",
                   "the Theil-Sen approach. For more information on the analysis ",
                   "see the 'Analysis Info' tab. "),
          helpText("Options for data metrics, trending methods, and handling missing ",
                   "dates can be found below. Additional years to exclude from the analysis ",
                   "(e.g. outliers) ",
                   "can be added below or by clicking or lassoing around points on ",
                   "the trending plot."),
          helpText("Click 'Compute' after making any changes to settings ",
                   "(including plot settings)."),
          # Compute button
          bsButton(ns("compute"), "Compute Trends", style = "primary",
                   class = "centreButton"),

          hr(class = "narrowHr"),

          h3("Options"),

          show_ui(ns("show_options"), "Data Metrics"),
          div(id = ns("options"),
              h5("Summary Statistics"),
              fluidRow(id = ns("percentiles_tip"),
                       column(6,
                              selectizeInput(ns("annual_percentiles"),
                                             label = "Annual Percentiles:",
                                             choices = c(0:100),
                                             selected = c(10,90),
                                             multiple = TRUE)),
                       column(6,
                              selectizeInput(ns("monthly_percentiles"),
                                             label = "Monthly Percentiles:",
                                             choices = c(0:100),
                                             selected = c(10,20),
                                             multiple = TRUE)),
                       bsTooltip(ns("percentiles_tip"), tips$percentiles,
                                 placement = "left")),

              h5("Low Flows"),
              select_rolling(id, name = "low_roll", multiple = TRUE),

              # h5("Flow Timing"),
              selectizeInput(ns("timing_percent"),
                             label = "Flow Timing - Percent of Total Annual Flows:",
                             choices = c(0:100),
                             selected = c(25, 33, 50, 75),
                             multiple = TRUE),
              bsTooltip(ns("timing_percent"), tips$percent, placement = "left"),

              # h5("Normal Days"),
              sliderInput(ns("normal_percentiles"),
                          label = "Normal Days - Percentiles Range:",
                          value = c(25, 75), min = 0, max = 100, step = 1),
              bsTooltip(ns("normal_percentiles"), tips$normal_percentiles, placement = "left")
          ),

          show_ui(ns("show_methods"), "Trend Method"),
          fluidRow(id = ns("methods"),
                   column(width = 6, id = ns("zyp_tip"),
                          awesomeRadio(ns("zyp"),
                                       label = "Prewhitening method:",
                                       choices = list("Zhang" = "zhang",
                                                      "Yue-Pilon" = "yuepilon"),
                                       selected = "zhang")),
                   column(width = 6, id = ns("alpha_tip"),
                          numericInput(ns("alpha"), label = "Trend alpha:",
                                       value = 0.05, min = 0, max = 0.3, step = 0.05)),
                   bsTooltip(ns("zyp_tip"), tips$zyp, placement = "left"),
                   bsTooltip(ns("alpha_tip"), tips$alpha, placement = "left")),

          show_ui(ns("show_allowed"), "Missing Dates"),
          div(id = ns("allowed"), uiOutput(ns("ui_allowed"))),

          # Other options
          br(),
          uiOutput(ns("ui_exclude")),
          hr(),
          ui_download(id = ns("plot")), br(),
          helpText("Note: the discharge unit type provided to this analysis is always ",
                   "in cubic metres per seconds (cms) due to unit conversions. So if ",
                   "'Volumetric Discharge' or 'Runoff Yield' data types have been selected ",
                   "on the Data >> Loading tab, ",
                   "the trending analysis is still using the original Discharge (cms) data."),
      ),

      tabBox(
        width = 9, height = 900,

        ### Plot/Table ---------------------
        tabPanel(
          title = "Exploring Trends",
          h4(textOutput(ns("table_fit_title"))),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table_fit"))),
          p(style = "margin-bottom:30px"), # A bit of space
          ui_plot_selection(id),
          ggiraph::girafeOutput(ns("plot"), height = "450px")),

        ### Trends Plot ---------------------
        tabPanel(
          title = "Plot - Results",
          "put the new plot here, with dropdown to select 'group' as it would be too large otherwise"
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table - Annual Values",
          h4(textOutput(ns("table_years_title"))),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table_years")))
        ),

        ### Info ---------------------
        tabPanel(
          title = "Analysis Info",
          "trend alpha"
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_annual_trends <- function(id, data_settings, data_raw,
                                 data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements -------------------------
    # Excluded years, takes defaults from data_settings()$years_exclude,
    # but allowed to modify here
    output$ui_exclude <- renderUI({
      req(data_settings()$years_range)
      tagList(
        selectizeInput(NS(id, "years_exclude"),
                       label = "Years to Exclude:",
                       choices = seq(from = data_settings()$years_range[1],
                                     to = data_settings()$years_range[2], by = 1),
                       selected = data_settings()$years_exclude,
                       multiple = TRUE),
        bsTooltip(id = "years_exclude", title = tips$years_exclude,
                  placement = "left"))
    })

    # Update years_exclude as points selected/unselected
    observe({
      updateNumericInput(inputId = "years_exclude",
                         value = c(excluded(), input$plot_selected))
    }) %>%
      bindEvent(input$plot_selected, ignoreNULL = FALSE)

    output$ui_allowed <- renderUI({
      tagList(
        sliderInput(NS(id, "allowed_annual"),
                    label = "Annual - Percent Allowed Missing:",
                    value = data_settings()$allowed, step = 5, min = 0, max = 100),
        sliderInput(NS(id, "allowed_monthly"),
                    label = "Monthly - Percent Allowed Missing:",
                    value = data_settings()$allowed, step = 5, min = 0, max = 100),
        bsTooltip(NS(id, "allowed_annual"), tips$allowed, placement = "left"),
        bsTooltip(NS(id, "allowed_monthly"), tips$allowed, placement = "left")
      )
    })

    # Preserve dynamic UI inputs during bookmarking
    setBookmarkExclude(c("compute")) # Set inputs, but user must click button
    keep <- c("allowed_annual", "allowed_monthly", "years_exclude")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))


    # General toggles
    observe(shinyjs::toggle("methods", condition = input$show_methods))
    observe(shinyjs::toggle("options", condition = input$show_options))
    observe(shinyjs::toggle("allowed", condition = input$show_allowed))

    # Change button status -----------------------

    # Current settings
    settings_current <- reactive({
      s <- get_inputs(input, which = c(
        "years_exclude",
        "zyp", "alpha", "annual_percentiles", "monthly_percentiles",
        "low_roll_days", "low_roll_align", "timing_percent", "normal_percentiles",
        "allowed_annual", "allowed_monthly"))
      s$data_raw <- data_raw()
      s$data_settings <- data_settings()
      s
    })

    # Settings at last Compute
    settings_last <- reactive(settings_current()) %>% bindEvent(input$compute)

    observe({
      settings_current()
      # Change buttons and record status if changes
      if(input$compute > 0) {
        update_on_change(session, id,
                         current = settings_current(), last = settings_last(),
                         labels = paste0("Compute Analysis<br><small>",
                                         c("Settings/Data have changed",
                                           "No changes since last computation"),
                                         "</small>"))
      }
    })



    # Excluded ----------------------------
    # What years were excluded when the trends were last calculated?
    excluded <- reactive({
      input$years_exclude
    }) %>%
      bindEvent(input$compute)

    # Trends -----------------------
    trends <- reactive({
      req(input$zyp)

      data_flow <- data_raw()

      # ignore_missing / allowed missing
      #basin area?

      # Define parameters
      # - These parameters are based on data_settings (roll_...), but have
      #  different arguments names (stats_days), so need to be kept as "extra"
      p <- c(glue::glue("stats_days = {data_settings()$roll_days}"),
             glue::glue("stats_align = '{data_settings()$roll_align}'")) %>%
        glue::glue_collapse(sep = ", ")

      r <- create_fun(
        fun = "compute_annual_trends", data_name = "data_flow", input,
        input_data = data_settings(), extra = p, params_ignore = "discharge")

      code$data <- r
      labels$data <- "Compute Annual Trends (creates all outputs as a list)"

      eval_check(r)
    }) %>%
      bindEvent(input$compute)

    # Table - Fit -----------------------
    output$table_fit <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Trends'"))

      req(trends())

      isolate({
        s <- input$table_fit_rows_selected
        if(is.null(s)) s <- 1
      })


      trends_headerCallback <- c(
        "function(thead, data, start, end, display){",
        "  var tooltips = [
        'STATION_NUMBER',
        'Annual Statistic',
        'the lower bound of the trends 95 percent confidence interval',
        'the Sens slope (trend) per year',
        'the Sens slope (trend) over the time period',
        'the upper bound of the trends 95 percent confidence interval',
        'Kendalls tau statistic computed on the final detrended timeseries',
        'Kendalls P-value computed for the final detrended timeseries',
        'the number of runs required to converge upon a trend',
        'the autocorrelation of the final detrended timeseries',
        'the fraction of the data which is valid (not NA) once autocorrelation is removed',
        'the least squares fit trend on the same data',
        'the intercept of the Sens slope (trend)'
        ];",
        "  for(var i=0; i<13; i++){",
        "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
        "  }",
        "}"
      )

      trends()[["Annual_Trends_Results"]] %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), ~round(., 4))) %>%
        DT::datatable(rownames = FALSE,
                      filter = 'top',
                      extensions = c("Scroller", "Buttons"),
                      options = list(scrollX = TRUE, scrollY = 300, scroller = TRUE,
                                     deferRender = TRUE, dom = 'Brtip',
                                     buttons = list(list(extend = 'copy', title = NULL),
                                                    'csv', 'excel'),
                                     headerCallback = DT::JS(trends_headerCallback)),
                      selection = list(target = "row", mode = "single", selected = s))
    })

    output$table_fit_title <- renderText(title(data_settings(), "Trending Results"))


    # Stat - to plot ---------------------
    stat <- reactive({
      req(input$table_fit_rows_selected)
      trends()[["Annual_Trends_Results"]] %>%
        dplyr::slice(input$table_fit_rows_selected) %>%
        dplyr::pull(.data$Statistic) %>%
        as.character()
    })


    # Plot --------------------
    plot <- reactive({
      s <- stat()
      g <- trends()[[s]] +
        ggiraph::geom_point_interactive(ggplot2::aes(
          tooltip = glue::glue("Year: {.data$Year}\n",
                               "{.env$s}: {round(.data$Value, 4)}"),
          data_id = .data$Year), size = 4, na.rm = TRUE)

    })
    output$plot <- ggiraph::renderGirafe({

      ggiraph::girafe(ggobj = plot(),
                      width_svg = 10 * opts$scale,
                      height_svg = 5 * opts$scale,
                      options = ggiraph_opts(selection = "multiple"))
    })

    dims <- c(10, 5) * opts$scale
    # Download Plot -----------------
    download(id = "plot", plot = plot, name = paste0("trend_",stat()),
             data_settings, dims)

    # Add/Remove selected points if changing the numericInput
    observe({
      yrs <- input$years_exclude       # All excluded years
      yrs <- yrs[!yrs %in% excluded()] # Not ones excluded in last run (point doesn't exist)
      if(length(yrs) == 0) yrs <- NULL

      if(!identical(yrs, input$plot_selected)) {
        if(is.null(yrs)) yrs <- ""
        session$sendCustomMessage(type = 'plot_set', message = yrs)
      }
    }) %>%
      bindEvent(input$years_exclude, ignoreNULL = FALSE, ignoreInit = TRUE)


    # Table - years -----------------------
    output$table_years <- DT::renderDT(server = FALSE, {

      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data >> Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Trends'"))

      req(trends())

      prep_DT(trends()[[1]])

    })

    output$table_years_title <- renderText(title(data_settings(), "Annual Values"))

    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(id, output)

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
