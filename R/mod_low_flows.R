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
ui_low_flows <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Low Flows"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        select_rolling(id, set = FALSE, multiple = TRUE),
        uiOutput(ns("ui_display"))
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
          DT::DTOutput(ns("table"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_low_flows <- function(id, data_settings, data_raw,
                             data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ----------------------------
    # Plot display
    output$ui_display <- renderUI({
      req(plots())
      select_plot_display(id, plots())
    })

    # Plots --------------------
    plots <- reactive({
      data_flow <- data_raw()

      g <- create_fun(fun = "plot_annual_lowflows", data_name = "data_flow",
                      input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot Annual low flows"

      g <- eval_check(g)

      # Add interactivity
      d1 <- dplyr::left_join(g[[1]]$data, g[[2]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_doy")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year))

      g[["Annual_Low_Flows"]]$data <- d1
      g[["Annual_Low_Flows"]] <- g[["Annual_Low_Flows"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Year: {.data$Year}\n",
            "{.data$Statistic}\n",
            "Date: {.data$Date}\n",
            "Discharge: {round(.data$Value, 4)}"),
            data_id = .data$Year), size = 3)

      d2 <- dplyr::left_join(g[[2]]$data, g[[1]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_discharge")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year))

      g[["Annual_Low_Flows_Dates"]]$data <- d2
      g[["Annual_Low_Flows_Dates"]] <- g[["Annual_Low_Flows_Dates"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Year: {.data$Year}\n",
            "{.data$Statistic}\n",
            "Date: {.data$Date}\n",
            "Discharge: {round(.data$Value_discharge, 4)}"),
            data_id = .data$Year), size = 3)
      g
    })

    # Plot output -------------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$display, input$roll_days, input$display %in% names(plots()))

      g <- plots()[[input$display]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(
            data_settings(), stringr::str_replace_all(input$display, "_", " "))) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = 12))
      }

      r <- length(input$roll_days)
      dims <- dplyr::case_when(r == 1 ~ c(7, 5),
                               r == 2 ~ c(8, 7),
                               r == 3 ~ c(9, 8),
                               r == 4 ~ c(10, 8),
                               r == 5 ~ c(10, 12),
                               TRUE ~ c(10, 14))

      ggiraph::girafe(ggobj = g,
                      width_svg = dims[1] * opts$scale,
                      height_svg = dims[2]* opts$scale,
                      options = ggiraph_opts())
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun(fun = "calc_annual_lowflows", data_name = "data_flow",
                      input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate Annual low flows"

      eval_check(t) %>%
        prep_DT()
    })

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))
  })
}
