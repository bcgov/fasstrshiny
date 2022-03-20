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

# HYDAT Peaks ------------------
ui_hydat_peak <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("HYDAT Peak Volume Frequency Analysis"),
      box(width = 3,

          shinyBS::bsButton(ns("compute"), "Compute Analysis", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          hr(class = "narrowHr"),

          fluidRow(
            column(width = 6, id = ns("use_max_tip"),
                   awesomeRadio(ns("use_max"),
                                label = "Flow type",
                                choices = list("Low" = FALSE,
                                               "High" = TRUE),
                                selected = FALSE)),
            column(width = 6, id = ns("fit_distr_tip"),
                   awesomeRadio(ns("fit_distr"),
                                label = "Distribution",
                                choices = list("PIII" = "PIII",
                                               "Weibull" = "weibull")))
          ),
          shinyBS::bsTooltip(ns("use_max_tip"), tips$use_max, placement = "left"),
          shinyBS::bsTooltip(ns("fit_distr_tip"), tips$fit_distr, placement = "left"),

          selectizeInput(
            ns("fit_quantiles"),
            label = "Quantiles to estimate",
            choices = seq(0.01, 0.999, 0.0025),
            selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                         0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
            multiple = TRUE),
          shinyBS::bsTooltip(ns("fit_quantiles"), tips$fit_quantiles,
                             placement = "left"),

          fluidRow(
            column(width = 6, id = ns("plot_curve_tip"),
                   prettySwitch(ns("plot_curve"),
                                label = "Plot curve", value = TRUE,
                                status = "success", slim = TRUE)),
            column(width = 6, id = ns("use_log_tip"),
                   prettySwitch(ns("use_log"),
                                label = "Log trans", slim = TRUE,
                                value = FALSE, status = "success"))
          ),
          shinyBS::bsTooltip(ns("plot_curve_tip"), tips$plot_curve, placement = "left"),
          shinyBS::bsTooltip(ns("use_log_tip"), tips$use_log, placement = "left"),

          fluidRow(
            column(6, id = ns("prob_plot_tip"),
                   awesomeRadio(ns("prob_plot"),
                                label = "Plotting positions",
                                choices = list("Weibull" = "weibull",
                                               "Median" = "median",
                                               "Hazen" = "hazen"))),
            column(6, id = ns("prob_scale_tip"),
                   textInput(
                     ns("prob_scale"),
                     label = "Probabilies to plot",
                     value = paste0("0.9999, 0.999, 0.99, 0.9, 0.5, 0.2, 0.1, ",
                                    "0.02, 0.01, 0.001, .0001")))
          ),
          shinyBS::bsTooltip(ns("prob_plot_tip"), tips$prob_plot, placement = "left"),
          shinyBS::bsTooltip(ns("prob_scale_tip"), tips$prob_scale, placement = "left"),
      ),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          shinycssloaders::withSpinner(ggiraph::girafeOutput(ns("plot")))
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          shinycssloaders::withSpinner(DT::DTOutput(ns("table")))
        ),

        ### Plot ---------------------
        tabPanel(
          title = "Fit Checks",
          verbatimTextOutput(ns("fit_stats")),
          shinycssloaders::withSpinner(
            plotOutput(ns("fit_plot"), height = "550px"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_hydat_peak <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Change button status -----------------------

    # Current settings
    settings_current <- reactive({
      s <- get_inputs(input, which = c(
        "use_max", "fit_distr", "fit_quantiles",
        "plot_curve", "use_log",
        "prob_plot", "prob_scale"))
      s$data_raw <- data_raw()
      s$data_settings <- data_settings
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

    # Frequencies -----------------------
    freqs <- reactive({

      need(
        isTruthy(data_raw()$STATION_NUMBER) &
          length(unique(data_raw()$STATION_NUMBER)) == 1,
        paste0("This analysis is only available for HYDAT data with a ",
               "valid STATION_NUMBER")) %>%
        validate()


      need(all(!is.na(text_to_num(input$prob_scale))),
           "Probabilies to plot must be a comma separated list of numbers") %>%
        validate()

      data_flow <- data_raw()

      # Define parameters
      p <- c(
        glue::glue("station_number = '{unique(data_flow$STATION_NUMBER)}'"),
        glue::glue("use_max = {input$use_max}"),
        glue::glue("use_log = {input$use_log}"),
        glue::glue("prob_plot_position = '{input$prob_plot}'"),
        glue::glue("prob_scale_points = c({input$prob_scale})"),
        glue::glue("fit_distr = '{input$fit_distr}'"),
        glue::glue("fit_quantiles = c({glue::glue_collapse(input$fit_quantiles, sep = ', ')})"),
        glue::glue("plot_curve = {input$plot_curve}")) %>%
        glue::glue_collapse(sep = ", ")

      r <- create_fun(fun = "compute_hydat_peak_frequencies", input = input,
                      input_data = data_settings, extra = p)

      code$data <- r

      eval(parse(text = r))
    }) %>%
      bindEvent(input$compute)

    ## Plot --------------------
    output$plot <- ggiraph::renderGirafe({

      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      # Add interactivity
      g <- freqs()[["Freq_Plot"]] +
        ggiraph::geom_point_interactive(ggplot2::aes(
          tooltip = paste0("Year: ", Year, "\n",
                           "Probabily: ", round(prob, 4), "\n",
                           "Discharge: ", round(Value, 4), "\n",
                           "Return Period: ", round(Return_P)),
          data_id = Year), size = 4)

      ggiraph::girafe(ggobj = g,
                      options = list(ggiraph::opts_selection(type = "none"),
                                     ggiraph::opts_toolbar(position = "topleft")))
    })


    ## Table -----------------------
    output$table <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      prep_DT(freqs()[["Freq_Fitted_Quantiles"]])
    })

    ## Fit checks --------------------
    output$fit_stats <- renderPrint({
      freqs()[["Freq_Fitting"]][[1]]
    })

    output$fit_plot <- renderPlot({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))
      freqs()[["Freq_Fitting"]][[1]] %>%
        gg_fitdistr(title = "")
    })


    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(id, output)

    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}
