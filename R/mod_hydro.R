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
      width = 12, h2("Hydrographs and Long-term Statistics"),

      # Settings -----------------
      box(
        width = 3,
        helpText("Explore annual hydrographs, summarizing statistics derived from daily means or monthly means ",
                 "over all years of selected data. How the data is grouped, the range of percentile ribbons (for the plot), and ",
                 "additional percentiles to calculate (for the table) can be modified below."),hr(),
        # Analysis type
        div(align = "left",
            awesomeRadio(ns("type"), label = "Summary Type:",
                         choices = list("Daily Means by Day" = "Daily",
                                        "Daily Means by Month" = "Long-term Daily",
                                        "Monthly Means by Month" = "Long-term Monthly"),
                         selected = "Daily",
                         status = "primary")),
        bsTooltip(ns("type"), "Type of statistic to calculate",
                           placement = "left"),
        hr(),
        # Percentiles
        select_percentiles_slide(
          id, name = "inner_percentiles", label = "Inner Percentiles (plot):",
          value = default("plot_daily_stats", "inner_percentiles")),
        select_percentiles_slide(
          id, name = "outer_percentiles", label = "Outer Percentiles (plot):",
          value = default("plot_daily_stats", "outer_percentiles")),
        select_percentiles(
          id, name = "extra_percentiles", label = "Additional Percentiles (table):",
          selected = default("calc_daily_stats", "percentiles")),
        hr(),
        ui_download(id = ns("plot")),br(),
        helpText("Note: the 'Daily Means by Day' plot summarizes daily means for each day of the year (i.e. stats of all Jan 1 daily mean). ",
                 "The 'Daily Means by Month' plot summarizes daily means for each month (i.e. stats of all May daily means). ",
                 "The 'Monthly Means by Month' summarizes monthly means for each month (i.e. stats of all May monthly means).")
      ),

      # Outputs
      tabBox(
        width = 9,

        # Plot ---------------------
        tabPanel(
          title = "Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),

        # Table ---------------------
        tabPanel(
          title = "Table",
          h4(textOutput(ns("table_title"))),
          select_table_options(id, include = "custom_months"),
          DT::DTOutput(ns("table"))
        ),

        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}


server_hydro <- function(id, data_settings, data_raw,
                         data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    output$test <- renderPrint({
      req(input$inner_percentiles)
      is.null(input$inner_percentiles)
    })

    # UI Elements ------------------------------------------

    # Plot options
    output$ui_plot_options <- renderUI({
      req(data_settings()$years_range, data_settings()$discharge)
      select_plot_options(
        select_plot_title(id),
        select_plot_log(
          id, value = default("plot_longterm_daily_stats", "log_discharge")),
        select_plot_extremes(id),
        select_plot_inner_percentiles(id),
        select_plot_outer_percentiles(id),
        h4("Add to Plot:"),
        select_add_year(id, data_settings()$years_range), # Dynamically created from data
        select_add_dates(id),
        select_add_mad(id),
        selectizeInput(NS(id, "mad"),
                       label = "Percent of Mean Annual Discharge (MAD):",
                       choices = c(0:100),
                       selected = c(5, 10, 20),
                       multiple = TRUE),
        bsTooltip(NS(id, "mad"), tips$mad, placement = "left"),
        select_custom(id, values = data_raw()[[data_settings()$discharge]])
      )
    })

    # Preserve dynamic UI inputs during bookmarking
    keep <- c("plot_title",
              "plot_log", "plot_extremes", "add_year",
              "plot_inner_percentiles", "plot_outer_percentiles",
              "add_dates", "add_mad", "mad")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Enable/Disable based on toggle
    observe(shinyjs::toggleState("add_dates", condition = input$type == "Daily"))
    observe(shinyjs::toggleState("custom_months_all",
                                 condition = input$type != "Daily"))
    observe(shinyjs::toggleState("mad", condition = input$add_mad))
    observe(shinyjs::toggleState("custom", condition = input$add_custom))
    observe(shinyjs::toggleState("custom_label", condition = input$add_custom))


    # Titles ----------
    titles <- reactive(title(data_settings(), glue::glue("{input$type} Hydrograph")))

    # Plot --------------------
    plot <- reactive({
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
      labels$plot <- glue::glue("Plot {input$type} hydrographs")
      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }


      # Add interactivity
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in%  c("AnalysisDate",
                                    "LT_Mean", "LT_Med")] # Omit these

      # For tooltips labels...
      names(stats)[stats %in% c("Year_mean", "RollingValue")] <- input$add_year
      names(stats)[stats == "Date"] <- "Day"
      names(stats)[stats == "DayofYear"] <- ifelse(data_settings()$water_year == 1, "Day of Year", "Day of Water Year")

      # Add interactive vline
      g <- g + create_vline_interactive(
        data = g$data, stats = stats,
        size = dplyr::if_else(input$type == "Daily", 1, 20))

      # Add dates
      if(input$type == "Daily" & !is.null(input$add_dates)){
        dts <- data.frame(
          Date = get_date(input$add_dates,
                          water_year = as.numeric(data_settings()$water_year))) %>%
          dplyr::mutate(labs = format(.data$Date, '%b-%d'),
                        hjust = dplyr::if_else(
                          as.numeric(data_settings()$water_year) ==
                            as.numeric(format(.data$Date, "%m")),
                          -0.05, 1.05))

        g <- g +
          ggiraph::geom_vline_interactive(
            xintercept = dts$Date, colour = 'grey20', tooltip = dts$labs) +
          ggplot2::geom_text(data = dts, ggplot2::aes(x = .data$Date,
                                                      label = .data$labs,
                                                      hjust = .data$hjust),
                             y = Inf, vjust = 2)
      }

      # Add mad
      if(isTRUE(input$add_mad)) {
        g <- g +
          ggplot2::geom_hline(
            data = mad(),
            ggplot2::aes(yintercept = .data$value),
            size = c(1, rep(0.5, nrow(mad()) - 0.75)),
            linetype = "dashed") +
          ggiraph::geom_hline_interactive(
            data = mad(),
            ggplot2::aes(tooltip = paste0(
              stringr::str_replace(.data$type, "%", "% "),
              ": ", round(.data$value, 4)),
              yintercept = .data$value), alpha = 0.01,
            size = 3) +
          ggplot2::geom_text(
            data = mad(),
            ggplot2::aes(y = .data$value, label = .data$type),
            x = c(Inf, rep(-Inf, nrow(mad()) - 1)), colour = "black",
            hjust = c(1.1, rep(-0.1, nrow(mad()) -1)), vjust = -0.5)
      }

      # Add custom
      if(isTRUE(input$add_custom)) {
        g <- g +
          ggplot2::geom_hline(
            ggplot2::aes(yintercept = input$custom),
            size = 1) +
          ggiraph::geom_hline_interactive(
            ggplot2::aes(
              tooltip = glue::glue("{input$custom_label}: {round(input$custom, 2)}"),
              yintercept = input$custom),
            alpha = 0.01, size = 3) +
          ggplot2::geom_text(
            data = data.frame(y = round(input$custom, 2),
                              label = input$custom_label),
            ggplot2::aes(y = .data$y, label = .data$label), x = Inf,
            colour = "black", hjust = 1.1, vjust = -0.5)
      }

      g
    })

    dims <- c(12, 6) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts())
      })

    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0("hydro_", input$type)),
             data_settings, dims)

    # MAD -----------------------
    mad <- reactive({
      req(input$mad)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_longterm_mean",
        data_name = "data_flow", input, input_data = data_settings(),
        end = paste0(" %>% tidyr::pivot_longer(dplyr::contains(\"MAD\"), ",
                       "names_to = \"type\", values_to = \"value\")"))

      code$mad <- glue::glue("mad <- {t}")
      labels$mad <- "Calculate Mean Annual Dischrage (for adding to plot)"

      eval_check(t)
    })

    # Table -----------------------
    output$table <- DT::renderDT(server = FALSE, {
      check_data(data_loaded())
      req(input$type)

      data_flow <- data_raw()

      perc <- c(input$inner_percentiles,
                input$outer_percentiles,
                input$extra_percentiles) %>%
        unique()

      t <- switch(input$type,
                  "Long-term Daily" = "calc_longterm_daily_stats",
                  "Long-term Monthly" = "calc_longterm_monthly_stats",
                  "Daily" = "calc_daily_stats") %>%
        create_fun(
          data_name = "data_flow", input, input_data = data_settings(),
          params_ignore = "percentiles",
          extra = glue::glue(
            "percentiles = {conseq(perc)}"))

      code$table <- t
      labels$table <- glue::glue("Calculate {input$type} hydrograph statistics")

      eval_check(t) %>%
        prep_DT()
    })

    output$table_title <- renderText(titles())


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(
      code, labels, data_code,
      order = c("data_raw", "mad", "plot", "table")))

  })
}



