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
        bsTooltip(ns("normal_percentiles"), tips$normal_percentiles, placement = "left"),
        ui_download(id = ns("plot"))
      ),
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
          h4(textOutput(ns("table_title"))),
          DT::DTOutput(ns("table"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_outside_normal <- function(id, data_settings, data_raw,
                                  data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # Titles --------------------
    titles <- reactive(title(data_settings(), "Days Outside Normal"))

    # Plot --------------------
    plot <- reactive({
      check_data(data_loaded())
      req(input$normal_percentiles)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_outside_normal", data_name = "data_flow",
        input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot Annual periods of data outside the normal range"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = 12))
      }

      # Add interactivity
      g <- g + ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = glue::glue("Year: {.data$Year}\n",
                                          "{.data$Statistic}\n",
                                          "No. Days: {round(.data$Value, 4)}"),
                     data_id = .data$Year), size = 3)
    })


    dims <- c(12, 8) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts())
    })


    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "outside_normal", data_settings, dims)


    # Table -----------------------
    output$table <- DT::renderDT({
      req(input$normal_percentiles)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_annual_outside_normal", data_name = "data_flow",
        input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate Annual periods of data outside the normal range"

      eval_check(t) %>%
        prep_DT()
    })


    output$table_title <- renderText(titles())

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
