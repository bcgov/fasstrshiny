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

        # Summary Plot -----------------
        tabPanel(
          title = "Data Summary Plot",
          fluidRow(
            column(
              width = 3,
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
                choices = eval(formals(plot_data_screening)$include_stat),
                selected = eval(formals(plot_data_screening)$include_stat),
                multiple = TRUE, width = "100%")),
            column(width = 9,
                   ggiraph::girafeOutput(ns("plot_summary"),
                                         height = opts$plot_height)))),

        # Symbols Plot ----------------------
        tabPanel(
          title = "Symbols Plots",
          fluidRow(
            column(
              width = 3,
              fluidRow(
                column(
                  width = 6, id = ns("symbols_type_tip"),
                  awesomeRadio(ns("symbols_type"), label = "Plot type",
                               choices = c("Days", "Flow")),
                  bsTooltip(ns("symbols_type_tip"),
                                     paste0("Plot type to show: Flow by year or ",
                                            "Number/proportion of each symbol by year"),
                                     placement = "left")),
                column(
                  width = 6, id = ns("symbols_percent_tip"),
                  awesomeRadio(ns("symbols_percent"),
                               label = "Plot days",
                               choices = c("Number of days" = FALSE,
                                           "Percent of days" = TRUE),
                               selected = formals(plot_annual_symbols)$plot_percent),
                  bsTooltip(ns("symbols_percent_tip"),
                                     "Plot days as proportion rather than number",
                                     placement = "left"))
              ),
              strong("HYDAT data symbols are: "), br(),
              strong("'E'"), "Estimate", br(),
              strong("'A'"), "Partial Day", br(),
              strong("'C'"), "Ice Conditions", br(),
              strong("'D'"), "Dry", br(),
              strong("'R'"), "Revised"
            ),
            column(
              width = 9,
              uiOutput(ns("ui_plot_symbols_options"), align = "right"),
              shinycssloaders::withSpinner(
                ggiraph::girafeOutput(ns("plot_symbols"),
                                      height = opts$plot_height))
            ))),


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
            ),
            column(width = 11, ggiraph::girafeOutput(ns("plot_available"),
                                                     height = opts$plot_height))
          )
        ),

        # Table -----------------
        tabPanel(
          title = "Table",
          DT::dataTableOutput(ns("table"))
        ),

        # R Code -----------------
        ui_rcode(id)
      )
    )
  )
}


server_data_available <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI -------------
    # Add plot options as Gear in corner
    output$ui_plot_symbols_options <- renderUI({
      select_plot_options(
        select_plot_log(
          id, value = formals(plot_flow_data_symbols)$log_discharge), # Default
      )
    })

    observe(shinyjs::toggleState("symbols_percent",
                                 condition = input$symbols_type == "Days"))

    # Data --------------
    available_raw <- reactive({

      data_flow <- data_raw()

      d <- create_fun("screen_flow_data", data_name = "data_flow", input,
                      input_data = data_settings())

      code$data <- glue::glue("data_available <- {d}")
      eval(parse(text = d))
    })

    # Summary plot ------------------
    output$plot_summary <- ggiraph::renderGirafe({

      check_data(data_loaded())
      req(input$availability, !is.null(input$stats))

      data_flow <- data_raw()

      e <- c(glue::glue("include_stats = ",
                        "c(\"{glue::glue_collapse(input$stats, sep = '\", \"')}\")"),
             glue::glue("plot_availability = {input$availability}")) %>%
        glue::glue_collapse(sep = ", ")

      g <- create_fun("plot_data_screening", data_name = "data_flow",
                      input, input_data = data_settings(), extra = e)

      code$plot_summary <- g

      g <- eval(parse(text = g))[[1]]

      # Add interactivity
      stats <- names(g$data) # Get stats from plot data

      # For tooltips labels...
      names(stats)[stats == "n_missing_Q"] <- "Data Completeness"

      # Add interactive vline
      g <- g + create_vline_interactive(data = g$data, stats = stats, size = 5)


      ggiraph::girafe(
        ggobj = g, width_svg = 13, height_svg = 7,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(
            css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })

    # Symbols Plot -----------------------------
    output$plot_symbols <- ggiraph::renderGirafe({

      check_data(data_loaded())
      req(!is.null(input$plot_log), input$symbols_type,
          !is.null(input$symbols_percent))

      data_flow <- data_raw()

      g <- switch(input$symbols_type,
                  "Flow" = "plot_flow_data_symbols",
                  "Days" = "plot_annual_symbols") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings(),
                   params_ignore = "discharge",
                   extra = dplyr::if_else(
                     input$symbols_type == "Days",
                     glue::glue("plot_percent = {input$symbols_percent}"),
                     ""))

      code$plot_symbols <- g

      g <- eval(parse(text = g))[[1]]

      # Add interactivity
      if(input$symbols_type == "Flow") {
        stats <- c("Date", "Value", "Symbol")
        names(stats)[stats == "Value"] <- "Discharge"
        g <- g + create_vline_interactive(data = g$data, stats = stats)

      } else{
        d <- g$data %>%
          dplyr::mutate(tooltip = glue::glue(
            "{Symbol}: {Count} ({round(Percent, 1)}%)")) %>%
          dplyr::group_by(Year) %>%
          dplyr::summarize(Count = sum(Count),
                           tooltip = glue::glue("Year: {Year}\n",
                                                glue::glue_collapse(tooltip, "\n")))

        g <- g +
          ggiraph::geom_bar_interactive(
            data = d, fill = "grey", alpha = 0.005,
            stat = "identity", inherit.aes = FALSE,
            ggplot2::aes(x = Year, y = Inf, tooltip = tooltip, data_id = Year))
      }

      ggiraph::girafe(
        ggobj = g, width_svg = 13, height_svg = 7,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(
            css = "fill:orange; stroke:gray; fill-opacity:1;")))
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

      g <- eval(parse(text = g))[[1]]

      # Replace layers with interactive
      g$layers[[1]] <- ggiraph::geom_tile_interactive(
        colour = "grey",
        ggplot2::aes(fill = Percent_Missing,
                     tooltip = glue::glue(
                       "Year: {Year}\n",
                       "Month: {Month}\n",
                       "Missing Days: {Missing} ({Percent_Missing}%)",
                       .trim = FALSE),
                     data_id = glue::glue("{Year}-{Month}")))

      g <- g +
        ggplot2::guides(fill = ggplot2::guide_coloursteps(
          even.steps = FALSE,
          show.limits = TRUE))

      ggiraph::girafe(ggobj = g, width_svg = 14, height_svg = 7,
                      options = list(
                        ggiraph::opts_toolbar(position = "topleft"),
                        ggiraph::opts_selection(type = "none")))
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

    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))
  })
}
