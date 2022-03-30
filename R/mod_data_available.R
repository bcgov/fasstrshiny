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

# Data Availability ---------------------
ui_data_available <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Data Availability"),

      tabBox(
        width = 12,

        # Symbols Flow Plot -----------------
        tabPanel(
          title = "Symbols Flow Plot",
          fluidRow(
            column(
              width = 2,
              strong("HYDAT data symbols are: "), br(),
              strong("'E'"), "Estimate", br(),
              strong("'A'"), "Partial Day", br(),
              strong("'B'"), "Ice Conditions", br(),
              strong("'D'"), "Dry", br(),
              strong("'R'"), "Revised"
            ),
            column(
              width = 10,
              select_plot_options(
                select_plot_title(id, "plot_title_symbols_flow"),
                select_plot_log(
                  id, value = default("plot_flow_data_symbols",
                                      "log_discharge"))),


              # Plot
              ui_plotly_info(),
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("plot_symbols_flow"),
                                     height = opts$plot_height))))
          ),

        # Symbols Agg Plot ----------------------
        tabPanel(
          title = "Symbols Aggregate Plots",
          fluidRow(
            column(
              width = 3,
              div(id = ns("symbols_agg_type_tip"),
                  awesomeRadio(
                    ns("symbols_agg_type"), label = "Plot type",
                    choices = c("Day of Year" = "dayofyear",
                                "No. Days" = "count", "% Days" = "percent")),
                  bsTooltip(ns("symbols_agg_type_tip"),
                            paste0("Plot symbols by ",
                                   "Day of Year; by Number of Days; ",
                                   "or by Percent of Days"),
                            placement = "left")
              ),
              strong("HYDAT data symbols are: "), br(),
              strong("'E'"), "Estimate", br(),
              strong("'A'"), "Partial Day", br(),
              strong("'B'"), "Ice Conditions", br(),
              strong("'D'"), "Dry", br(),
              strong("'R'"), "Revised"
            ),
            column(
              width = 9,
              uiOutput(ns("ui_plot_symbols_agg_options"), align = "right"),
              select_plot_options(select_plot_title(id,
                                                    "plot_title_symbols_agg")),

              shinycssloaders::withSpinner(
                ggiraph::girafeOutput(ns("plot_symbols_agg"),
                                      height = opts$plot_height)))
            )),

        # Summary Plot -----------------
        tabPanel(
          title = "Data Summary Plot",
          fluidRow(
            column(width = 3,
              helpText("Placeholder descriptive text to describe this section, ",
                       "what it does and how to use it"),
              div(id = ns("availability_tip"),
                  prettySwitch(ns("availability"),
                               label = "Plot availability",
                               value = TRUE,
                               status = "success", slim = TRUE, inline = TRUE)),
              bsTooltip(ns("availability_tip"), tips$availability,
                        placement = "left"),
              selectizeInput(
                ns("stats"),
                label = "Statistics to include",
                choices = default("plot_data_screening", "include_stats"),
                selected = default("plot_data_screening", "include_stats"),
                multiple = TRUE, width = "100%"),
              bsTooltip(ns("stats"), tips$stats,
                        placement = "left"),

              strong("Note"), br(),
              "Statistics are calculated ignoring missing dates",

              ),
            column(width = 9,
                   select_plot_options(select_plot_title(id, "plot_title_summary")),
                   ggiraph::girafeOutput(ns("plot_summary"),
                                         height = opts$plot_height))
            )),


        # Availability Plot -----------------
        tabPanel(
          title = "Data Availability Plot",
          fluidRow(
            column(width = 1,
                   awesomeRadio(ns("available_type"), label = "Plot type",
                                choices = c("Tile" = "tile", "Bar" = "bar")),
                   checkboxGroupButtons(
                     ns("months_inc"),
                     label = "Months",
                     choices = list("Jan" = 1, "Feb" = 2,
                                    "Mar" = 3, "Apr" = 4,
                                    "May" = 5, "Jun" = 6,
                                    "Jul" = 7, "Aug" = 8,
                                    "Sep" = 9, "Oct" = 10,
                                    "Nov" = 11, "Dec" = 12),
                     selected = c(1:12),
                     direction = "vertical"),
                   bsTooltip(ns("months_inc"),
                             "Months to include/exclude from the plot",
                             placement = "left"),
                   bsButton(ns("months_all"), label = "All Months",
                            size = "small"),
                   bsTooltip(ns("months_all"), "Select all months",
                             placement = "left")
            ),
            column(width = 11,
                   select_plot_options(select_plot_title(id, "plot_title_available")),
                   ggiraph::girafeOutput(ns("plot_available"),
                                         height = opts$plot_height))
          )
        ),

        # Table -----------------
        tabPanel(
          title = "Table",
          h4(textOutput(ns("table_title"))),
          DT::dataTableOutput(ns("table"))
        ),

        # R Code -----------------
        ui_rcode(id)
      )
    )
  )
}


server_data_available <- function(id, data_settings, data_raw,
                                  data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements -------------
    observe(shinyjs::toggleState("symbols_percent",
                                 condition = input$symbols_type == "Days"))
    observe(shinyjs::toggleState("plot_log",
                                 condition = input$symbols_type == "Flow"))
    observe(updateCheckboxGroupButtons(session, "months_inc", selected = 1:12)) %>%
      bindEvent(input$months_all)

    # Data --------------
    available_raw <- reactive({

      data_flow <- data_raw()

      d <- create_fun("screen_flow_data", data_name = "data_flow", input,
                      input_data = data_settings())

      code$data <- glue::glue("data_available <- {d}")
      labels$data <- "Summarize statistics, missing values, symbols from flow data"
      eval_check(d)
    })

    # Symbols Flow Plot -----------------------------
    output$plot_symbols_flow <- plotly::renderPlotly({
      check_data(data_loaded())
      validate(need("Symbol" %in% names(data_raw()),
                    "Cannot plot unless there is a 'Symbol' column in the data"))

      data_flow <- data_raw()

      g <- create_fun("plot_flow_data_symbols", data_name = "data_flow", input,
                      input_data = data_settings(), params_ignore = "discharge")

      code$plot_symbols_flow <- g
      labels$plot_symbols_flow <- glue::glue("Plot Symbols by flow")

      g <- eval_check(g)[[1]]


      # Add title
      if(input$plot_title_symbols_flow) {
        g <- g +
          ggplot2::ggtitle(title(
            data_settings(), glue::glue("Flow with symbols"))) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      plotly::ggplotly(g, dynamicTicks = TRUE) %>%
        plotly::config(modeBarButtonsToRemove =
                         c("pan", "autoscale", "zoomIn2d", "zoomOut2d",
                           "lasso2d", "select2d",
                           "hoverCompareCartesian", "hoverClosestCartesian"))
    }) %>%
      bindCache(data_raw(), data_settings())

    # Symbols Agg Plot -----------------------------
    output$plot_symbols_agg <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$symbols_agg_type)
      validate(need("Symbol" %in% names(data_raw()),
                    "Cannot plot unless there is a 'Symbol' column in the data"))

      data_flow <- data_raw()

      g <- create_fun("plot_annual_symbols", data_name = "data_flow", input,
                      input_data = data_settings(), params_ignore = "discharge",
                      extra = glue::glue("plot_type = '{input$symbols_agg_type}'"))

      code$plot_symbols_agg <- g
      labels$plot_symbols_agg <- glue::glue("Plot aggregates of symbols")

      g <- eval_check(g)[[1]]


      # Add title
      if(input$plot_title_symbols_agg) {
        g <- g +
          ggplot2::ggtitle(title(
            data_settings(), glue::glue("Aggregate symbols"))) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      if(input$symbols_agg_type == "dayofyear") {
        d <- g$data %>%
          dplyr::mutate(tooltip = glue::glue("Date: {.data$Date}"),
                        tooltip = dplyr::if_else(
                          !is.na(.data$Value),
                          glue::glue("{tooltip}\nSymbol: {.data$Symbol}"),
                          glue::glue("{tooltip}\nMissing value")))

        g <- g +
          ggiraph::geom_tile_interactive(
            data = d,
            ggplot2::aes(tooltip = .data$tooltip, data_id = .data$Date))
      } else {
        d <- g$data %>%
          dplyr::mutate(tooltip = glue::glue(
            "{Symbol}: {Count} ({round(Percent, 1)}%)")) %>%
          dplyr::group_by(.data$Year) %>%
          dplyr::summarize(Count = sum(.data$Count),
                           tooltip = glue::glue(
                             "Year: {.data$Year}\n",
                             glue::glue_collapse(.data$tooltip, "\n")))

        g <- g +
          ggiraph::geom_bar_interactive(
            data = d, fill = "grey", alpha = 0.005,
            stat = "identity", inherit.aes = FALSE,
            ggplot2::aes(x = .data$Year, y = Inf,
                         tooltip = .data$tooltip, data_id = .data$Year))
      }

      ggiraph::girafe(ggobj = g,
                      width_svg = 13 * opts$scale,
                      height_svg = 7 * opts$scale,
                      options = ggiraph_opts())

    })

    # Summary plot ------------------
    output$plot_summary <- ggiraph::renderGirafe({

      check_data(data_loaded())
      req(!is.null(input$availability), !is.null(input$stats))

      data_flow <- data_raw()

      g <- create_fun("plot_data_screening", data_name = "data_flow",
                      input, input_data = data_settings())

      code$plot_summary <- g
      labels$plot_summary <- "Plot summary statistics for data screening"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title_summary) {
        g <- g +
          ggplot2::ggtitle(title(data_settings(), "Data Availability")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      stats <- names(g$data) # Get stats from plot data

      # For tooltips labels...
      names(stats)[stats == "n_missing_Q"] <- "Data Completeness"

      # Add interactive vline
      g <- g + create_vline_interactive(data = g$data, stats = stats, size = 5)


      ggiraph::girafe(ggobj = g,
                      width_svg = 13 * opts$scale,
                      height_svg = 7 * opts$scale,
                      options = ggiraph_opts())
    })



    # Available Data Plot ---------------------------
    output$plot_available <- ggiraph::renderGirafe({

      check_data(data_loaded())
      req(input$available_type, input$months_inc)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_missing_dates", data_name = "data_flow",
        input, input_data = data_settings(),
        params_ignore = c("discharge", "months"),
        extra = glue::glue(
          "months = c({glue::glue_collapse(input$months_inc, sep = ', ')}), ",
          "plot_type = '{input$available_type}'"))

      code$plot_available <- g
      labels$plot_available <- "Plot missing dates"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title_available) {
        g <- g +
          ggplot2::ggtitle(title(data_settings(), "Missing Data")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Replace layers with interactive
      if(input$available_type == "tile") {
        g$layers[[1]] <- ggiraph::geom_tile_interactive(
          colour = "grey",
          ggplot2::aes(fill = .data$Percent_Missing,
                       tooltip = glue::glue(
                         "Year: {.data$Year}\n",
                         "Month: {.data$Month}\n",
                         "Missing Days: {.data$Missing} ({.data$Percent_Missing}%)",
                         .trim = FALSE),
                       data_id = glue::glue("{.data$Year}-{.data$Month}")))

        g <- g +
          ggplot2::guides(fill = ggplot2::guide_coloursteps(
            even.steps = FALSE,
            show.limits = TRUE))
      } else {
        # Add interactivity
        stats <- names(g$data) # Get stats from plot data

        # For tooltips labels...
        names(stats)[stats == "Value"] <- "No. Missing Days"

        g <- g + create_vline_interactive(data = g$data, stats = stats, size = 5)
      }

      ggiraph::girafe(ggobj = g,
                      width_svg = 14 * opts$scale,
                      height_svg = 7 * opts$scale,
                      options = ggiraph_opts())
    })

    # Summary table ------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      available_raw() %>%
        dplyr::select(-dplyr::contains("STATION_NUMBER")) %>%
        dplyr::rename("Total days" = "n_days",
                      "Total flow days" = "n_Q",
                      "Total missing days" = "n_missing_Q",
                      "Standard deviation" = "StandardDeviation") %>%
        dplyr::rename_with(
          .cols = dplyr::ends_with("_missing_Q"),
          ~ stringr::str_replace(., "_missing_Q", " missing days")) %>%
        prep_DT()
    })

    output$table_title <- renderText({
      title(data_settings(), "Screening")
    })

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(
      code, labels, data_code,
      order = c("data_raw", "plot_symbols_flow", "plot_symbols_agg",
                "plot_summary", "plot_available", "data")))
  })

}
