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



# Volume Frequency - High/Low ------------------
ui_volume_freq <- function(id, plot_height) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("High/Low Volume Frequency Analysis"),
      box(width = 3,

          # Buttons
          bsButton(ns("compute"), "Compute Analysis", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          hr(class = "narrowHr"),

          # Other
          uiOutput(ns("ui_exclude")),

          show_ui(ns("show_data"), "Data"),
          div(id = ns("data"),
              select_rolling(id, set = FALSE, multiple = TRUE),

              fluidRow(
                column(
                  width = 6,
                  awesomeRadio(ns("use_max"),
                               label = "Flow type",
                               choices = list("Low" = FALSE,
                                              "High" = TRUE),
                               selected = FALSE, inline = TRUE)),
                column(
                  width = 6, id = ns("use_log_tip"),
                  prettySwitch(
                    ns("use_log"),
                    label = tags$span(strong("Log trans")),
                    value = FALSE, status = "success", slim = TRUE)),
                bsTooltip(ns("use_max"), tips$use_max, placement = "left"),
                bsTooltip(ns("use_log_tip"), tips$use_log, placement = "left")
              )
          ),
          show_ui(ns("show_plotting"), "Plotting"),
          div(id = ns("plotting"),
              fluidRow(
                column(6,
                       awesomeRadio(ns("prob_plot"),
                                    label = "Plotting positions",
                                    choices = list("Weibull" = "weibull",
                                                   "Median" = "median",
                                                   "Hazen" = "hazen"))),
                column(6, textInput(
                  ns("prob_scale"),
                  label = "Probabilies to plot",
                  value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001"))
              ),
              div(id = ns("plot_curve_tip"),
                  prettySwitch(ns("plot_curve"),
                               label = tags$span(strong("Plot curve")),
                               value = TRUE, status = "success", slim = TRUE)),
              bsTooltip(ns("plot_curve_tip"), tips$plot_curve, placement = "left"),
              bsTooltip(ns("prob_plot"), tips$prob_plot, placement = "left"),
              bsTooltip(ns("prob_scale"), tips$prob_scale, placement = "left")
          ),

          show_ui(ns("show_fitting"), "Fitting"),
          div(id = ns("fitting"),
              selectizeInput(
                ns("fit_quantiles"),
                label = "Quantiles to estimate",
                choices = seq(0.01, 0.999, 0.0025),
                selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                             0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
                multiple = TRUE),
              awesomeRadio(ns("fit_distr"),
                           label = "Distribution",
                           choices = list("PIII" = "PIII",
                                          "Weibull" = "weibull")),
              awesomeRadio(ns("fit_distr_method"),
                           label = "Distribution method",
                           choices = list("Method of Moments (MOM)" = "MOM",
                                          "Maximum Likelihood Estimation (MLE)" = "MLE")),

              bsTooltip(ns("fit_quantiles"), tips$fit_quantiles, placement = "left"),
              bsTooltip(ns("fit_distr"), tips$fit_distr, placement = "left"),
              bsTooltip(ns("fit_distr_method"), tips$fit_distr_method, placement = "left")
          )
      ),

      tabBox(
        width = 9,

        # Plot ---------------------
        tabPanel(
          title = "Plot",
          conditionalPanel(
            "output.plot", ns = NS(id),
            helpText("Click on a point to add that year to ",
                     "'Years to exclude'. Remember to re-Compute ",
                     "Analysis.")),
          withSpinner(ggiraph::girafeOutput(ns("plot")))
        ),

        # Table - Plot Data ---------------------
        tabPanel(
          title = "Table - Plot Data",
          withSpinner(DT::DTOutput(ns("table_plot")))
        ),

        # Table - Fitted Quantiles ---------------------
        tabPanel(
          title = "Table - Fitted Quantiles",
          withSpinner(DT::DTOutput(ns("table_fit")))
        ),

        # Fit Plot ---------------------
        tabPanel(
          title = "Fit Checks",
          uiOutput(ns("ui_day")),
          verbatimTextOutput(ns("fit_stats")),
          withSpinner(plotOutput(ns("fit_plot"), height = "550px"))
        ),

        # Info ---------------------
        tabPanel(
          title = "Analysis Info"
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_volume_freq <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Excluded years, takes defaults from data_settings$years_exclude,
    # but allowed to modify here
    output$ui_exclude <- renderUI({
      req(data_settings$years_range)
      selectizeInput(NS(id, "years_exclude"),
                     label = "Years to exclude",
                     choices = seq(from = data_settings$years_range[1],
                                   to = data_settings$years_range[2], by = 1),
                     selected = data_settings$years_exclude,
                     multiple = TRUE)
    })

    # Update years_exclude as points selected/unselected
    observe({
      updateNumericInput(inputId = "years_exclude",
                         value = c(excluded(), input$plot_selected))
    }) %>%
      bindEvent(input$plot_selected)

    output$ui_day <- renderUI({
      radioGroupButtons(NS(id, "day"),
                        choices = names(freqs()$Freq_Fitting),
                        selected = names(freqs()$Freq_Fitting)[1])
    })

    observe(toggle("data", condition = input$show_data))
    observe(toggle("plotting", condition = input$show_plotting))
    observe(toggle("fitting", condition = input$show_fitting))



    # Excluded ----------------------------
    # What years were excluded when the trends were last calculated?
    excluded <- reactive({
      input$years_exclude
    }) %>%
      bindEvent(input$compute)

    # Frequencies -----------------------
    freqs <- reactive({

      validate(need(all(!is.na(text_to_num(input$prob_scale))),
                    "Probabilies to plot must be a comma separated list of numbers"))

      data_flow <- data_raw()

      # Define parameters
      p <- c(
        glue::glue("exclude_years = c({glue::glue_collapse(input$years_exclude, sep = ', ')})"),
        glue::glue("use_max = {input$use_max}"),
        glue::glue("use_log = {input$use_log}"),
        glue::glue("prob_plot_position = '{input$prob_plot}'"),
        glue::glue("prob_scale_points = c({input$prob_scale})"),
        glue::glue("fit_distr = '{input$fit_distr}'"),
        glue::glue("fit_distr_method = '{input$fit_distr_method}'"),
        glue::glue("fit_quantiles = c({glue::glue_collapse(input$fit_quantiles, sep = ', ')})"),
        glue::glue("plot_curve = {input$plot_curve}")) %>%
        glue::glue_collapse(sep = ", ")

      r <- create_fun(fun = "compute_annual_frequencies", data = "data_flow",
                      input, input_data = data_settings,
                      extra = p, params_ignore = "years_exclude")

      code$data <- r

      eval(parse(text = r))
    }) %>%
      bindEvent(input$compute)

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({

      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      g <- freqs()[["Freq_Plot"]] +
        ggiraph::geom_point_interactive(ggplot2::aes(
          tooltip = paste0("Year: ", Year, "\n",
                           "Discharge", ": ", round(Value, 4), "\n",
                           "Probability", ": ", round(prob, 4)),
          data_id = Year), size = 3) +
        ggplot2::scale_colour_viridis_d(end = 0.8)

      ggiraph::girafe(ggobj = g, width_svg = 8, height_svg = 5,
                      options = list(ggiraph::opts_selection(type = "multiple"),
                                     ggiraph::opts_toolbar(position = "topleft")))
    })

    # Remove selected points if changing the numericInput
    observe({
      yrs <- input$years_exclude       # All excluded years
      yrs <- yrs[!yrs %in% excluded()] # Not ones excluded in last run (point doesn't exist)

      if(length(yrs) == 0) yrs <- NULL
      if(!identical(yrs, input$plot_selected)) { # Don't change if no change to make
        if(is.null(yrs)) yrs <- ""
        session$sendCustomMessage(type = 'plot_set', message = yrs)
      }
    }) %>%
      bindEvent(input$years_exclude, ignoreNULL = FALSE, ignoreInit = TRUE)






    # Table - Plot data -----------------------
    output$table_plot <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      prep_DT(freqs()[["Freq_Plot_Data"]])
    })

    # Table - Fitted Quantiles -----------------------
    output$table_fit <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      prep_DT(freqs()[["Freq_Fitted_Quantiles"]])
    })

    # Fit checks --------------------
    output$fit_stats <- renderPrint({
      req(input$day)
      freqs()[["Freq_Fitting"]][[input$day]]
    })

    output$fit_plot <- renderPlot({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      req(input$day)
      freqs()[["Freq_Fitting"]][[input$day]] %>%
        gg_fitdistr(title = input$day)
    })





    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(id, output)

    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}
