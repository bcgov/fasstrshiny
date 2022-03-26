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


ui_flows <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Flow Duration and Percentiles"),

      ## Settings -----------------------------
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        div(id = ns("longterm_tip"),
            prettySwitch(ns("longterm"),
                         label = "Include Long-term",
                         value = TRUE,
                         status = "success", slim = TRUE)),
        bsTooltip(ns("longterm_tip"), tips$longterm, placement = "left"),
        uiOutput(ns("ui_months")),
        bsTooltip(ns("ui_months"), "Months to include/exclude from the plot",
                           placement = "left"),
        select_custom_months(id),

        # Update button
        bsButton(ns("compute"), "Update", style = "primary",
                          class = "centreButton"),
        helpText("Click 'Update' after making any changes to settings ",
                 "(including plot settings)")
      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot - Flow duration",
          select_plot_options(
            select_plot_title(id),
            select_plot_log(id, value = default("plot_flow_duration",
                                                "log_discharge"))),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(ns("plot"), height = opts$plot_height))
        ),

        ## Table ---------------------
        tabPanel(
          title = "Table - Percentiles",
          shinycssloaders::withSpinner(DT::DTOutput(ns("table")))
        ),

        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_flows <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI Elements -----------------------------------------------

    output$ui_months <- renderUI({
      checkboxGroupButtons(
        NS(id, "months"),
        label = "Months to plot",
        choices = list("Jan" = 1, "Feb" = 2,
                       "Mar" = 3, "Apr" = 4,
                       "May" = 5, "Jun" = 6,
                       "Jul" = 7, "Aug" = 8,
                       "Sep" = 9, "Oct" = 10,
                       "Nov" = 11, "Dec" = 12),
        selected = data_settings()$months)
    })


    # Preserve dynamic UI inputs during bookmarking
    keep <- c("months")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Plot --------------------

    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_flow_duration", data_name = "data_flow",
                      input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot flow duration"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(data_settings(), "Flow Duration")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Month: {Month}\n",
            "{Percentile}% time\n",
            "Discharge cutoff: {round(Value, 3)}",
            .trim = FALSE),
            data_id = Percentile),
          show.legend = FALSE, alpha = 0.01, size = 3)

      ggiraph::girafe(ggobj = g, width_svg = 12, height = 6,
                      options = ggiraph_opts())
    }) %>%
      bindEvent(input$compute, ignoreNULL = FALSE)


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_longterm_daily_stats",
        data_name = "data_flow", input, input_data = data_settings(),
        extra = "percentiles = 1:99",
        end = "%>% dplyr::select(-Mean, -Median, -Minimum, -Maximum)")

      code$table <- t
      labels$table <- "Calculate flows duration"

      eval_check(t) %>%
        tidyr::pivot_longer(cols = -c(STATION_NUMBER, Month),
                            names_to = "percentiles", values_to = "value") %>%
        tidyr::pivot_wider(names_from = Month, values_from = value) %>%
        prep_DT()
    }) %>%
      bindEvent(input$compute, ignoreNULL = FALSE)


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels))

  })
}
