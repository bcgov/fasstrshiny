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

ui_cumulative <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Cumulative Hydrographs"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        awesomeRadio(ns("type"),
                     label = "Cumulative type",
                     choices = list("Daily", "Monthly"),
                     status = "primary"),
        bsTooltip(ns("type"), "Type of cumulative statistics to calculate",
                           placement = "left"),
        awesomeRadio(ns("discharge2"),
                     label = "Discharge type",
                     choices = list("Volumetric Discharge (m3)" = FALSE,
                                    "Runoff Yield (mm)" = TRUE),
                     selected = TRUE),
        bsTooltip(ns("discharge2"), tips$discharge2, placement = "left")
      ),
      tabBox(
        width = 9,

        # Plot ---------------------
        tabPanel(
          title = "Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),

        # Table ---------------------
        tabPanel(
          title = "Table",
          select_table_options(id, include = "percentiles"),
          DT::DTOutput(ns("table"))
        ),

        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}


server_cumulative <- function(id, data_settings, data_raw, data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ------------
    output$ui_plot_options <- renderUI({
      req(data_settings()$years_range)
      select_plot_options(
        select_plot_title(id),
        select_plot_log(
          id, value = default("plot_daily_cumulative_stats", "log_discharge")),
        select_add_year(id, data_settings()$years_range))
    })

    # Preserve dynamic UI inputs during bookmarking
    keep <- c("plot_title", "plot_log", "add_year")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$type, !is.null(input$add_year), input$discharge2)

      data_flow <- data_raw()

      g <- switch(input$type,
                  "Monthly" = "plot_monthly_cumulative_stats",
                  "Daily"   = "plot_daily_cumulative_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings())

      code$plot <- g
      labels$plot <- glue::glue("Plot {input$type} cumulative hydrographs")

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(
            data_settings(),
            glue::glue("{input$type} Cumulative Hydrograph")))+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("WaterYear", "AnalysisDate", "DayofYear")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Monthly_Total"] <- input$add_year
      names(stats)[stats == "Cumul_Flow"] <- input$add_year

      # Add interactive vline
      g <- g + create_vline_interactive(
        data = g$data, stats = stats,
        size = dplyr::if_else(input$type == "Monthly", 20, 1))


      ggiraph::girafe(
        ggobj = g, width_svg = 14, height_svg = 6,
        options = ggiraph_opts())
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$discharge2)

      data_flow <- data_raw()

      t <- switch(input$type,
                  "Monthly" = "calc_monthly_cumulative_stats",
                  "Daily"   = "calc_daily_cumulative_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings())

      code$table <- t
      labels$table <- glue::glue("Calculate {input$type} cumulative hydrograph statistics")

      eval_check(t) %>%
        prep_DT()
    })

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}





