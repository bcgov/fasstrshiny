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

# Flow timing ------------------------------
ui_flow_timing <- function(id, plot_height) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Flow Timing"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        div(style = "max-width: 300px;",
            selectizeInput(ns("percent"),
                           label = "Percents of total annual flows",
                           choices = c(1:99),
                           selected = c(25, 33, 50, 75),
                           multiple = TRUE),
            bsTooltip(ns("percent"), tips$percent, placement = "left")
        )),
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

server_flow_timing <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_flow_timing", data = "data_flow",
        input, input_data = data_settings,
        extra = glue::glue("percent_total = ",
                           "c({glue::glue_collapse(input$percent, sep = ',')})"))

      code$plot <- g

      # Add interactivity
      g <- eval(parse(text = g))[[1]]

      g <- g +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Year: {Year}\n",
            "{Statistic}\n",
            "Date: {lubridate::as_date(",
            "Value, origin = paste0(Year, '-01-01')) - 1}"),
            data_id = Year), size = 3)

      ggiraph::girafe(ggobj = g, width_svg = 13, height_svg = 7,
                      options = list(
                        ggiraph::opts_toolbar(position = "topleft"),
                        ggiraph::opts_selection(type = "none")))
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_annual_flow_timing", data = "data_flow",
        input, input_data = data_settings,
        extra = glue::glue("percent_total = ",
                           "c({glue::glue_collapse(input$percent, sep = ',')})"))

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
