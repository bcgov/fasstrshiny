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

# Hydrographs and Long-term -------------------------------------------------

ui_hydro <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Daily and Long-term Hydrographs"),

      ## Settings -----------------
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),

        # Analysis type
        div(align = "left",
            awesomeRadio(ns("type"), label = "Summary type",
                         choices = list("Daily",
                                        "Long-term Daily",
                                        "Long-term Monthly"),
                         selected = "Daily",
                         status = "primary")),
        bsTooltip(ns("type"), "Type of statistic to calculate",
                           placement = "left"),

        # MAD Percents
        selectizeInput(ns("mad"),
                       label = "Percent of Mean Annual Discharge (MAD)",
                       choices = c(1:99),
                       selected = c(5, 10, 20),
                       multiple = TRUE),
        bsTooltip(ns("mad"), tips$mad, placement = "left"),

        # Percentiles
        select_percentiles(
          id, name = "inner_percentiles", label = "Inner percentiles",
          selected = default("plot_daily_stats", "inner_percentiles")),
        select_percentiles(
          id, name = "outer_percentiles", label = "Outer percentiles",
          selected = default("plot_daily_stats", "outer_percentiles")),
        select_percentiles(
          id, name = "extra_percentiles", label = "Additional percentiles (table)",
          selected = default("calc_daily_stats", "percentiles"))
      ),

      # Outputs
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),

        ## Table ---------------------
        tabPanel(
          title = "Table",
          select_table_options(id, include = "custom_months"),
          DT::DTOutput(ns("table"))
        ),

        ## R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}


server_hydro <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ------------------------------------------
    # Plot options
    output$ui_plot_options <- renderUI({
      select_plot_options(
        select_plot_log(
          id, value = default("plot_longterm_daily_stats", "log_discharge")),
        select_plot_extremes(id),
        select_add_year(id, data_settings()$years_range),
        select_add_dates(id),
        select_add_mad(id))
    })

    # Enable/Disable based on toggle
    observe(shinyjs::toggleState("add_dates", condition = input$type == "Daily"))
    observe(shinyjs::toggleState("custom_months_all",
                                 condition = input$type != "Daily"))


    ## Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      validate(
        need(length(input$inner_percentiles) %in% c(0, 2) &
               length(input$outer_percentiles) %in% c(0, 2),
             glue::glue("Inner and outer percentiles must each have two ",
                        "(or no) values, corresponding to the limits of the ",
                        "plot ribbons")))
      req(input$type, !is.null(input$add_year))

      data_flow <- data_raw()

      g <- switch(input$type,
                  "Daily" = "plot_daily_stats",
                  "Long-term Monthly" = "plot_longterm_monthly_stats",
                  "Long-term Daily" = "plot_longterm_daily_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings())

      code$plot <- g
      g <- eval_check(g)[[1]]

      # Add interactivity
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in%  c("DayofYear", "AnalysisDate",
                                    "LT_Mean", "LT_Med")] # Omit these

      # For tooltips labels...
      names(stats)[stats %in% c("Year_mean", "RollingValue")] <- input$add_year
      names(stats)[stats == "Date"] <- "Day"

      # Add interactive vline
      g <- g + create_vline_interactive(
        data = g$data, stats = stats,
        size = dplyr::if_else(input$type == "Daily", 1, 20))

      # Add dates
      if(input$type == "Daily" & !is.null(input$add_dates)){
        dts <- data.frame(
          Date = get_date(input$add_dates,
                          water_year = as.numeric(data_settings()$water_year))) %>%
          dplyr::mutate(labs = format(Date, '%b-%d'),
                        hjust = dplyr::if_else(
                          as.numeric(data_settings()$water_year) ==
                            as.numeric(format(Date, "%m")),
                          -0.05, 1.05))

        g <- g +
          ggiraph::geom_vline_interactive(
            xintercept = dts$Date, colour = 'grey20', tooltip = dts$labs) +
          ggplot2::geom_text(data = dts, ggplot2::aes(x = Date, label = labs,
                                                      hjust = hjust),
                             y = Inf, vjust = 2)
      }

      # Add mad
      if(isTRUE(input$add_mad)) {
        mad <- tidyr::pivot_longer(mad(), -STATION_NUMBER, names_to = "type")

        g <- g +
          ggplot2::geom_hline(
            data = mad,
            ggplot2::aes(yintercept = value),
            size = c(2, rep(1, nrow(mad) - 1))) +
          ggiraph::geom_hline_interactive(
            data = mad,
            ggplot2::aes(tooltip = paste0(stringr::str_replace(type, "%", "% "),
                                          ": ", round(value, 4)),
                         yintercept = value), alpha = 0.01,
            size = 3) +
          ggplot2::geom_text(
            data = mad,
            ggplot2::aes(y = value, label = type),
            x = c(Inf, rep(-Inf, nrow(mad) - 1)), colour = "black",
            hjust = c(1.1, rep(-0.1, nrow(mad) -1)), vjust = -0.5)
      }

      ggiraph::girafe(
        ggobj = g, width_svg = 12, height = 6,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(
            css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })



    ## MAD -----------------------
    mad <- reactive({
      req(input$mad)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_longterm_mean",
        data_name = "data_flow", input, input_data = data_settings())

      code$mad <- t

      eval_check(t)
    })

    ## Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$type)

      data_flow <- data_raw()

      perc <- c(input$inner_percentiles,
                input$outer_percentiles,
                input$extra_percentiles) %>%
        unique() %>%
        as.numeric() %>%
        sort()

      t <- switch(input$type,
                  "Long-term Daily" = "calc_longterm_daily_stats",
                  "Long-term Monthly" = "calc_longterm_monthly_stats",
                  "Daily" = "calc_daily_stats") %>%
        create_fun(
          data_name = "data_flow", input, input_data = data_settings(),
          params_ignore = "percentiles",
          extra = glue::glue(
            "percentiles = c({glue::glue_collapse(perc, sep = ', ')})"))

      code$table <- t

      eval_check(t) %>%
        prep_DT()
    })


    ## R Code -----------------
    code <- reactiveValues()
    output$code <- renderText({
      code_format(code)
    })

  })
}



