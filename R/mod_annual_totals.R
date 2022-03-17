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

ui_annual_totals <- function(id, plot_height) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Totals"),
      box(width = 3,
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          awesomeRadio(ns("discharge"),
                       label = "Discharge type",
                       choices = list("Volumetric Discharge (m3)" = FALSE,
                                      "Runoff Yield (mm)" = TRUE),
                       selected = TRUE),
          bsTooltip(ns("discharge"), tips$discharge, placement = "left"),
          uiOutput(ns("ui_display")),
          bsTooltip(ns("ui_display"),
                    paste0("Choose plot type to display.<br>",
                           "Seasonal plots are only available if all months ",
                           "are included<br>(see Data tab)"),
                    placement = "left")),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          ggiraph::girafeOutput(ns("plot"), height = plot_height)
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

    # UI plot display
    output$ui_display <- renderUI({
      req(plots())

      plot_names <- names(plots()) %>%
        setNames(., stringr::str_replace_all(., "_", " "))

      selectizeInput(NS(id, "display"), "Display plot",
                     choices = plot_names)
    })


    # Plots -------------------
    plots <- reactive({
      check_data(data_loaded())
      req(input$discharge)

      data_flow <- data_raw()
      g <- create_fun(fun = "plot_annual_cumulative_stats", data = "data_flow",
                      input, input_data = data_settings,
                      params_ignore = "discharge",
                      extra = glue::glue("use_yield = {input$discharge}, ",
                                         "include_seasons = TRUE"))

      code$plot <- g
      g <- eval(parse(text = g))

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
      req(input$display)

      g <- plots()[[input$display]]

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

      t <- create_fun("calc_annual_cumulative_stats", data = "data_flow",
                      input, input_data = data_settings,
                      params_ignore = "discharge",
                      extra = glue::glue("use_yield = {input$discharge}, ",
                                         "include_seasons = TRUE"))

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
