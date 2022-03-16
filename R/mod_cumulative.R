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

ui_cumulative <- function(id, plot_height) {
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
        awesomeRadio(ns("discharge"),
                     label = "Discharge type",
                     choices = list("Volumetric Discharge (m3)" = FALSE,
                                    "Runoff Yield (mm)" = TRUE),
                     selected = TRUE),
        bsTooltip(ns("discharge"), tips$discharge, placement = "left")
      ),
      tabBox(
        width = 9,

        # Plot ---------------------
        tabPanel(
          title = "Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          ggiraph::girafeOutput(ns("plot"), height = plot_height)
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


server_cumulative <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI Plot ------------
    output$ui_plot_options <- renderUI({
      select_plot_options(
        select_plot_log(
          id, value = formals(plot_daily_cumulative_stats)$log_discharge),
        select_add_year(id, data_settings$years_range))
    })


    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$type, !is.null(input$add_year))

      data_flow <- data_raw()

      e <- glue::glue("use_yield = {input$discharge}")
      if(input$add_year != "") {
        e <- glue::glue("{e}, add_year = {input$add_year}")
      }


      g <- switch(input$type,
                  "Monthly" = "plot_monthly_cumulative_stats",
                  "Daily"   = "plot_daily_cumulative_stats") %>%
        create_fun("data_flow", input, input_data = data_settings, extra = e,
                   params_ignore = "discharge")

      code$plot <- g

      # Add interactivity
      g <- eval(parse(text = g))[[1]]

      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("WaterYear", "AnalysisDate", "DayofYear")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Monthly_Total"] <- input$add_year
      names(stats)[stats == "Cumul_Flow"] <- input$add_year

      # Add interactive vline
      g <- g + create_vline_interactive(
        data = g$data, stats = stats,
        size = if_else(input$type == "Monthly", 20, 1))


      ggiraph::girafe(
        ggobj = g, width_svg = 14, height_svg = 6,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$discharge)

      data_flow <- data_raw()

      t <- switch(input$type,
                  "Monthly" = "calc_monthly_cumulative_stats",
                  "Daily"   = "calc_daily_cumulative_stats") %>%
        create_fun("data_flow", input, input_data = data_settings,
                   params_ignore = "discharge",
                   extra = glue::glue("use_yield = {input$discharge}"))

      code$table <- t

      parse(text = t) %>%
        eval() %>%
        prep_DT()
    })

    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}




