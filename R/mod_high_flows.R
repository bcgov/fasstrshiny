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

# High flows ------------------------
ui_high_flows <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual High Flows and Dates of High Flows"),
      box(
        width = 3,
        helpText("Explore annual high flows (maximums) and their dates' of occurrence. ",
                 "The duration of rolling day averages (and alignment) and ",
                 "the type of plot to display can be modifed below. "),hr(),
        select_rolling(id, multiple = TRUE),
        uiOutput(ns("ui_display")),hr(),
        ui_download(id = ns("plot")), br(),
        helpText("Note: the date of occurrence depends on the alignment of the rolling average. ",
                 "If 'Right' the day represents the last of n-days (i.e. with a 7-day average, ",
                 "July 7 represents the mean of July 1-7). If 'Left', the day represents ",
                 "the first of n-days (i.e. with a 7-day average, July 7 represents the mean ",
                 "of July 7-13. If 'Center', the day is centered on n-days (i.e. with a 7-day ",
                 "average, July 7 represents the mean of July 4-10)."
        )

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

server_high_flows <- function(id, data_settings, data_raw,
                              data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ----------------------------
    # Plot display
    output$ui_display <- renderUI({
      browser()
      req(plots())
      select_plot_display(id, plots())

    })

    # Plots --------------------
    plots <- reactive({
      browser()
      check_data(data_loaded())

      req(input$roll_days)

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_annual_highflows", data_name = "data_flow",
                      input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot Annual High Flows"

      g <- eval_check(g)

      # Add interactivity
      d1 <- dplyr::left_join(g[[1]]$data, g[[2]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_doy")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year, data_settings()$water_year))

      g[["Annual_High_Flows"]]$data <- d1
      g[["Annual_High_Flows"]] <- g[["Annual_High_Flows"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "{.data$Statistic}\n",
            "Year: {.data$Year}\n",
            "Date: {.data$Date}\n",
            "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$Value_doy}\n",
            "Discharge: {round(.data$Value, 4)}"),
            data_id = .data$Year), size = 3)

      d2 <- dplyr::left_join(g[[2]]$data, g[[1]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_discharge")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year, data_settings()$water_year))

      g[["Annual_High_Flows_Dates"]]$data <- d2
      g[["Annual_High_Flows_Dates"]] <- g[["Annual_High_Flows_Dates"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "{.data$Statistic}\n",
            "Year: {.data$Year}\n",
            "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$Value}\n",
            "Date: {.data$Date}\n",
            "Discharge: {round(.data$Value_discharge, 4)}"),
            data_id = .data$Year), size = 3)

      # Add title
      if(input$plot_title) {
        for(i in seq_len(length(g))) {
          g[[i]] <- g[[i]] +
            ggplot2::ggtitle(title(
              data_settings(), stringr::str_replace_all(names(g)[i], "_", " "))) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = 12))
        }
      }

      g
    })


    plot <- reactive({
      browser()
      req(input$display, input$display %in% names(plots()))
      plots()[[input$display]]
    })

    # Plot output -------------------------

    dims <- reactive({
      r <- length(input$roll_days)
      dplyr::case_when(r == 1 ~ c(7, 5),
                       r == 2 ~ c(8, 7),
                       r == 3 ~ c(9, 8),
                       r == 4 ~ c(10, 8),
                       r == 5 ~ c(10, 12),
                       TRUE ~ c(10, 14)) * opts$scale
    })

    output$plot <- ggiraph::renderGirafe({
      browser()
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims()[1] ,
                      height_svg = dims()[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0("high_flows_", input$display)),
             data_settings, dims)


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun(fun = "calc_annual_highflows", data_name = "data_flow",
                      input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate Annual High flows"

      eval_check(t) %>%
        prep_DT()
    })

    output$table_title <- renderText(title(data_settings(), "High Flows"))

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))
  })
}
