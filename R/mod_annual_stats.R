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


# Annual Statistics ------------------------------------------------
ui_annual_stats <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Statistics"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        div(align = "left",
            awesomeRadio(ns("type"),
                         label = "Summary type",
                         choices = list("Monthly",
                                        "Annual"),
                         selected = "Monthly",
                         status = "primary")),
        bsTooltip(ns("type"), "Type of statistic to calculate", placement = "left"),

        # Percentiles
        select_percentiles(
          id, name = "inner_percentiles", label = "Inner percentiles",
          selected = default("plot_daily_stats", "inner_percentiles")),
        select_percentiles(
          id, name = "outer_percentiles", label = "Outer percentiles",
          selected = default("plot_daily_stats", "outer_percentiles")),
        select_percentiles(
          id, name = "extra_percentiles", label = "Additional percentiles (table)",
          selected = default("calc_daily_stats", "percentiles")),

        # Months
        checkboxGroupButtons(
          ns("months_plot"),
          label = "Months to plot",
          choices = list("Jan" = 1, "Feb" = 2,
                         "Mar" = 3, "Apr" = 4,
                         "May" = 5, "Jun" = 6,
                         "Jul" = 7, "Aug" = 8,
                         "Sep" = 9, "Oct" = 10,
                         "Nov" = 11, "Dec" = 12),
          selected = c(1:12)),
        bsTooltip(ns("months_plot"),
                  paste0("Months to include/exclude from Monthly calculations<br>",
                         "(Annual uses default months from the Data tab)"),
                  placement = "left"),
      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot",
          select_plot_options(
            select_plot_title(id),
            select_plot_log(id, value = default("plot_monthly_stats2",
                                                "log_discharge"))),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),

        ## Table ---------------------
        tabPanel(
          title = "Table",
          DT::DTOutput(ns("table"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_annual_stats <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ------------------
    observe(shinyjs::toggleState("months_plot",
                                 condition = input$type == "Monthly"))

    # Plot -----------------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())

      validate(
        need(length(input$inner_percentiles) %in% c(0, 2) &
               length(input$outer_percentiles) %in% c(0, 2),
             glue::glue("Inner and outer percentiles must each have two ",
                        "(or no) values, corresponding to the limits of the ",
                        "plot ribbons")))

      req(!is.null(input$plot_log), input$type)

      data_flow <- data_raw()

      if(input$type == "Monthly") {
        pi <- "months"
        e <- glue::glue(
          "months = {conseq(input$months_plot)}")
      } else {
        pi <- NULL
        e <- ""
      }

      g <- switch(input$type,
                  "Monthly" = "plot_monthly_stats2",
                  "Annual" = "plot_annual_stats2") %>%
        create_fun(data_name = "data_flow", input,
                   input_data = data_settings(),
                   params_ignore = pi, extra = e)

      code$plot <- g
      labels$plot <- glue::glue("Plot {input$type} hydrograph statistics")

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(
            data_settings(), glue::glue("{input$type} Statistics"))) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      date_cols <- "Year"
      if(input$type == "Monthly") date_cols <- c(date_cols, "Month")
      stats <- names(g$data) # Get stats from plot data
      #stats <- stats[!stats %in% date_cols] # Omit these

      # Add vline
      g <- g + create_vline_interactive(
        data = g$data, stats, size = dplyr::if_else(input$type == "Annual", 10, 2))

      ggiraph::girafe(
        ggobj = g, width_svg = 12, height = 6,
        options = ggiraph_opts())
    })

    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$type)

      data_flow <- data_raw()

      perc <- c(input$inner_percentiles,
                input$outer_percentiles,
                input$extra_percentiles) %>%
        unique() %>%
        as.numeric() %>%
        sort()

      pi <- "percentiles"
      e <- glue::glue("percentiles = c({glue::glue_collapse(perc, sep = ', ')})")

      if(input$type == "Monthly") {
        pi <- c(pi, "months")
        e <- glue::glue(
          "{e}, months = c({glue::glue_collapse(input$months_plot, sep = ', ')})")
      }

      t <- switch(input$type,
                  "Monthly" = "calc_monthly_stats",
                  "Annual" = "calc_annual_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings(),
                   params_ignore = pi, extra = e)

      code$table <- t
      labels$table <- glue::glue("Calculate {input$type} hydrograph statistics")

      eval_check(t) %>%
        prep_DT()
    })


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels))

  })
}
