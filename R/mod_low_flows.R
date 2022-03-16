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


# Low flows ------------------------
ui_low_flows <- function(id, plot_height) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Low Flows"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        select_rolling(id, set = FALSE, multiple = TRUE)
      ),
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

server_low_flows <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_annual_lowflows", data = "data_flow",
                      input, input_data = data_settings)

      code$plot <- g

      # Add interactivity
      g <- eval(parse(text = g))

      g[["Annual_Low_Flows"]] <- g[["Annual_Low_Flows"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                        Statistic, "\n",
                                        "Discharge: ", round(Value, 4)),
                       data_id = Year), size = 3)

      g[["Annual_Low_Flows_Dates"]] <- g[["Annual_Low_Flows_Dates"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                        Statistic, "\n",
                                        "Day of Year: ", round(Value, 4)),
                       data_id = Year), size = 3)

      # Combine plots
      g <- patchwork::wrap_plots(g)

      ggiraph::girafe(ggobj = g, width_svg = 12, height_svg = 8,
                      options = list(
                        ggiraph::opts_toolbar(position = "topleft"),
                        ggiraph::opts_selection(type = "none")))
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun(fun = "calc_annual_lowflows", data = "data_flow",
                      input, input_data = data_settings)

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
