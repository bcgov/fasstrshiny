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
ui_flow_timing <- function(id) {

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

server_flow_timing <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_flow_timing", data_name = "data_flow",
        input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot Annual flow timings"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(data_settings(), "Flow Timing")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Year: {Year}\n",
            "{Statistic}\n",
            "Date: {yday_as_date(Value, Year)}"),
            data_id = Year), size = 3)

      ggiraph::girafe(ggobj = g, width_svg = 13, height_svg = 7,
                      options = ggiraph_opts())
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_annual_flow_timing", data_name = "data_flow",
        input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate Annual flow timings"

      eval_check(t) %>%
        prep_DT()
    })



    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels))

  })
}
