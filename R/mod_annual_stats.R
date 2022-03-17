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
ui_annual_stats <- function(id, plot_height) {

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
        bsTooltip(ns("type"), "Type of statistic to calculate", placement = "left")
      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          ggiraph::girafeOutput(ns("plot"), height = plot_height)
        ),

        ## Table ---------------------
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

server_annual_stats <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI Plot options ------------------
    output$ui_plot_options <- renderUI({
      select_plot_options(
        select_plot_log(id, value = formals(plot_monthly_stats2)$log_discharge))
    })

    # Plot -----------------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(!is.null(input$plot_log), input$type)

      data_flow <- data_raw()

      g <- switch(input$type,
                  "Monthly" = "plot_monthly_stats2",
                  "Annual" = "plot_annual_stats2") %>%
        create_fun("data_flow", input, input_data = data_settings)

      code$plot <- g

      g <- eval(parse(text = g))[[1]]


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
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })

    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$type, input$percentiles)

      data_flow <- data_raw()

      t <- switch(input$type,
                  "Monthly" = "calc_monthly_stats",
                  "Annual" = "calc_annual_stats") %>%
        create_fun("data_flow", input, input_data = data_settings)

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
