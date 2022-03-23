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

          bsButton(ns("compute"), "Compute Analysis", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          hr(class = "narrowHr"),

          show_ui(ns("show_data"), "Data"),
          div(id = ns("data"), select_analysis_data(id)),

          show_ui(ns("show_plotting"), "Plotting"),
          select_analysis_plots(id),

          show_ui(ns("show_fitting"), "Fitting"),
          select_fitting(id)
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

    # UI -----------------------
    observe(shinyjs::toggle("data", condition = input$show_data))
    observe(shinyjs::toggle("plotting", condition = input$show_plotting))
    observe(shinyjs::toggle("fitting", condition = input$show_fitting))

    # Change button status -----------------------

    # Current settings
    settings_current <- reactive({
      s <- get_inputs(input, which = c(
        "use_max", "fit_distr", "fit_quantiles",
        "plot_curve", "use_log",
        "prob_plot", "prob_scale"))
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

    # Frequencies -----------------------
    freqs <- reactive({

      # Inputs
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

      r <- create_fun(
        fun = "compute_hydat_peak_frequencies", input = input,
        input_data = data_settings(),
        extra = glue::glue(
          "station_number = '{unique(data_flow$STATION_NUMBER)}'"))

      code$data <- r
      eval_check(r)
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
