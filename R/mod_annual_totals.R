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

# Annual Totals -------------------------------

ui_annual_totals <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Totals"),
      box(width = 3,
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          awesomeRadio(ns("discharge2"),
                       label = "Discharge type",
                       choices = list("Volumetric Discharge (m3)" = FALSE,
                                      "Runoff Yield (mm)" = TRUE),
                       selected = TRUE),
          bsTooltip(ns("discharge2"), tips$discharge2,
                             placement = "left"),
          uiOutput(ns("ui_display"))),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          select_plot_options(select_plot_title(id)),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),

        ### Table ---------------------
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

server_annual_totals <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI Elements --------------------------------------
    output$ui_display <- renderUI({
      req(plots())
      select_plot_display(id, plots())
    })


    # Plots -------------------
    plots <- reactive({
      check_data(data_loaded())
      req(!is.null(input$discharge2))

      data_flow <- data_raw()
      g <- create_fun(fun = "plot_annual_cumulative_stats",
                      data_name = "data_flow",
                      input, input_data = data_settings(),
                      extra = "include_seasons = TRUE")

      code$plot <- g
      g <- eval_check(g)

      # Add interactivity
      for(i in seq_along(g)) {
        g[[i]] <- g[[i]] + ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Year: {Year}\n",
            "{stringr::str_remove(Statistic, '_$')}: {round(Value, 4)}",
            .trim = FALSE),
            data_id = Year), size = 3)
      }

      g
    })

    # Plot output --------------------
    output$plot <- ggiraph::renderGirafe({
      req(input$display, input$display %in% names(plots()))

      g <- plots()[[input$display]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(
            plot_title(data_settings(),
                       stringr::str_replace(input$display, "_", " "))) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      h <- switch(stringr::str_extract(input$display, "^[^_]+"),
                  "Two" = 8,
                  "Four" = 10,
                  "Total" = 5)

      ggiraph::girafe(
        ggobj = g, width_svg = 10, height_svg = h,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(
            css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun("calc_annual_cumulative_stats",
                      data_name = "data_flow",
                      input, input_data = data_settings(),
                      extra = "include_seasons = TRUE")

      code$table <- t

      eval_check(t) %>%
        prep_DT()
    })


    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}
