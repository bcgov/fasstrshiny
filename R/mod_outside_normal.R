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


# Days outside normal ------------------------
ui_outside_normal <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Days Outside Normal"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        sliderInput(ns("normal_percentiles"), label = "Normal range ",
                    value = c(25, 75), min = 1, max = 99, step = 1),
        bsTooltip(ns("normal_percentiles"), tips$normal_percentiles, placement = "left")
      ),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
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

server_outside_normal <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$normal_percentiles)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_outside_normal", data_name = "data_flow",
        input, input_data = data_settings())

      code$plot <- g

      # Add interactivity
      g <- eval_check(g)[[1]]

      g <- g + ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                      Statistic, "\n",
                                      "No. Days: ", round(Value, 4)),
                     data_id = Year), size = 3)

      ggiraph::girafe(
        ggobj = g, width_svg = 12, height_svg = 8,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      req(input$normal_percentiles)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_annual_outside_normal", data_name = "data_flow",
        input, input_data = data_settings())

      code$table <- t

      eval_check(t) %>%
        prep_DT()
    })



    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}
