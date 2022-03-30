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
          uiOutput(ns("ui_display")),
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
          DT::DTOutput(ns("table"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_annual_totals <- function(id, data_settings, data_raw,
                                 data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements --------------------------------------
    output$ui_display <- renderUI({
      req(plots())
      select_plot_display(id, plots())
    })


    # Plots -------------------
    plots <- reactive({
      req(!is.null(input$discharge2))
      check_yield(data_settings(), input$discharge2, require = TRUE)
      check_data(data_loaded())

      data_flow <- data_raw()
      g <- create_fun(fun = "plot_annual_cumulative_stats",
                      data_name = "data_flow",
                      input, input_data = data_settings(),
                      extra = "include_seasons = TRUE")

      code$plot <- g
      labels$plot <- "Plot Annual cumulative hydrograph statistics"
      g <- eval_check(g)

      # Add interactivity
      for(i in seq_along(g)) {
        g[[i]] <- g[[i]] + ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Year: {.data$Year}\n",
            "{stringr::str_remove(.data$Statistic, '_$')}: {round(.data$Value, 4)}",
            .trim = FALSE),
            data_id = .data$Year), size = 3)
      }

      # Add title
      if(input$plot_title) {
        for(i in seq_len(length(g))) {
          g[[i]] <- g[[i]] +
            ggplot2::ggtitle(
              plot_title(data_settings(),
                         stringr::str_replace_all(names(g)[i], "_", " "))) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
        }
      }

      g
    })

    plot <- reactive({
      req(input$display, input$display %in% names(plots()))
      plots()[[input$display]]
    })

    # Plot output --------------------
    dims <- reactive({
      req(!is.null(input$display))
      h <- switch(stringr::str_extract(input$display, "^[^_]+"),
             "Two" = 8,
             "Four" = 10,
             "Total" = 5)

      c(10, h) * opts$scale
    })

    output$plot <- ggiraph::renderGirafe({
      check_yield(data_settings(), input$discharge2)
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims()[1],
                      height_svg = dims()[2],
                      options = ggiraph_opts())
    })


    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0("annual_totals_", input$display)),
             data_settings, dims)


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      check_yield(data_settings(), input$discharge2)

      data_flow <- data_raw()

      t <- create_fun("calc_annual_cumulative_stats",
                      data_name = "data_flow",
                      input, input_data = data_settings(),
                      extra = "include_seasons = TRUE")

      code$table <- t
      labels$table <- "Calculate Annual cumulative hydrograph statistics"

      eval_check(t) %>%
        prep_DT()
    })


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
