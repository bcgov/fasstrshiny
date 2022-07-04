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

ui_cumulative <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Cumulative Hydrographs"),
      box(
        width = 3,
        helpText("Explore annual cumulative hydrographs, summarizing statistics ",
                 "derived from cumulative daily totals or monthly totals ",
                 "over all years of selected data. ",
                 "Volumetric discharge totals the daily discharge values ",
                 "for a given time period. Total yield converts the total volume ",
                 "to units in depth derived from the provided basin area. ",
                 "How the data is grouped and ",
                 "whether to choose units of total volumetric (cubic metres) or ",
                 "total yield (millimetres) can be modified below."),hr(),
        awesomeRadio(ns("type"),
                     label = "Cumulative Type:",
                     choices = list("Daily Totals by Day" = "Daily",
                                    "Monthly Totals by Month" = "Monthly"),
                     status = "primary"),
        bsTooltip(ns("type"), "Type of cumulative statistics to calculate",
                           placement = "left"),
        awesomeRadio(ns("discharge2"),
                     label = "Discharge Type:",
                     choices = list("Volumetric Discharge (m3)" = FALSE,
                                    "Runoff Yield (mm)" = TRUE),
                     selected = TRUE),
        bsTooltip(ns("discharge2"), tips$discharge2, placement = "left"),hr(),
        ui_download(id = ns("plot")),br(),
        helpText("Note: the 'Daily Totals by Day' plot summarizes cumulative daily ",
                 "totals for each year (i.e. stats for May 1 being the total ",
                 "cumulative discharge from Jan 1 to May 1). ",
                 "The 'Monthly Totals by Month' summarizes cumulative monthly totals ",
                 "for each month (i.e. stats of for May being the total ",
                 "cumulative discharge of monthly totals from Jan-May)."),
        helpText("Only years of complete data will be used for analysis.")

      ),
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
          select_table_options(id, include = "percentiles"),
          DT::DTOutput(ns("table"))
        ),

        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}


server_cumulative <- function(id, data_settings, data_raw, data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ------------
    output$ui_plot_options <- renderUI({
      req(data_settings()$years_range)
      select_plot_options(
        select_plot_title(id),
        select_plot_log(
          id, value = default("plot_daily_cumulative_stats", "log_discharge")),
        select_add_dates(id),
        select_add_year(id, data_settings()$years_range))
    })

    observe(shinyjs::toggleState("add_dates", condition = input$type == "Daily"))

    # Preserve dynamic UI inputs during bookmarking
    keep <- c("plot_title", "plot_log", "add_year", "add_dates")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Titles -------------
    titles <- reactive({
      title(data_settings(), glue::glue("{input$type} Cumulative Hydrograph"))
    })

    # Plot --------------------
    plot <- reactive({
      check_data(data_loaded())
      req(input$type, !is.null(input$add_year), input$discharge2)
      check_yield(data_settings(), input$discharge2)

      data_flow <- data_raw()

      g <- switch(input$type,
                  "Monthly" = "plot_monthly_cumulative_stats",
                  "Daily"   = "plot_daily_cumulative_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings())

      code$plot <- g
      labels$plot <- glue::glue("Plot {input$type} cumulative hydrographs")

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles())+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("WaterYear", "AnalysisDate")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Monthly_Total"] <- input$add_year
      names(stats)[stats == "Cumul_Flow"] <- input$add_year
      names(stats)[stats == "DayofYear"] <- ifelse(data_settings()$water_year == 1, "Day of Year", "Day of Water Year")

      # Add interactive vline
      g <- g + create_vline_interactive(
        data = g$data, stats = stats,
        size = dplyr::if_else(input$type == "Monthly", 20, 1))

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


      g
    })

    dims <- c(14, 6) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0("cumulative_", input$type)),
             data_settings, dims)


    # Table -----------------------
    output$table <- DT::renderDT(server = FALSE, {
      check_data(data_loaded())
      req(input$discharge2)
      check_yield(data_settings(), input$discharge2)

      data_flow <- data_raw()

      t <- switch(input$type,
                  "Monthly" = "calc_monthly_cumulative_stats",
                  "Daily"   = "calc_daily_cumulative_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings())

      code$table <- t
      labels$table <- glue::glue(
        "Calculate {input$type} cumulative hydrograph statistics")

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





