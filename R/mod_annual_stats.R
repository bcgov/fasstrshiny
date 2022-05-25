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


# Annual Statistics ------------------------------------------------
ui_annual_stats <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Summary Statistics"),
      box(
        width = 3,
        helpText("Explore annual means, minimums, maximums, and percentiles. ",
                 "The range of percentile ribbons (for the plot), ",
                 "additional percentiles to calculate (for the table) and the ",
                 "months to include on the 'Monthly' plot can be modified below."),
        hr(),
        div(align = "left",
            awesomeRadio(ns("type"),
                         label = "Summary Type",
                         choices = list("Annual", "Monthly"),
                         selected = "Annual",
                         status = "primary")),
        bsTooltip(ns("type"), "Type of statistic to calculate", placement = "left"),
        hr(),
        textOutput(ns("test")),
        # Percentiles
        select_percentiles_slide(
          id, name = "inner_percentiles", label = "Inner Percentiles (plot)",
          value = default("plot_daily_stats", "inner_percentiles")),
        select_percentiles_slide(
          id, name = "outer_percentiles", label = "Outer Percentiles (plot)",
          value = default("plot_daily_stats", "outer_percentiles")),
        select_percentiles(
          id, name = "extra_percentiles", label = "Additional Percentiles (table)",
          selected = default("calc_daily_stats", "percentiles")),
        hr(),
        # Months
        conditionalPanel(
          "input.type == 'Monthly'", ns = NS(id),
          checkboxGroupButtons(
            ns("months_plot"),
            label = "Months to Plot",
            choices = list("Jan" = 1, "Feb" = 2,
                           "Mar" = 3, "Apr" = 4,
                           "May" = 5, "Jun" = 6,
                           "Jul" = 7, "Aug" = 8,
                           "Sep" = 9, "Oct" = 10,
                           "Nov" = 11, "Dec" = 12),
            selected = c(1:12)),
          bsTooltip(ns("months_plot"),
                    paste0("Months to include/exclude from Monthly calculations<br>",
                           "(Annual uses default months from the Data tab)"),
                    placement = "left"),
          hr()),
        fluidRow(
          column(width = 6, ui_download(id = ns("plot"), name = "Download Distribution Plot")),
          column(width = 6, ui_download(id = ns("plot_line"), name = "Download Statistics Plot"))
        )

      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot - Distribution",
          select_plot_options(
            select_plot_title(id),
            select_plot_log(id, value = default("plot_monthly_stats2",
                                                "log_discharge"),
                            name = "plot_log_all"),
            select_plot_extremes(id),
            select_plot_inner_percentiles(id),
            select_plot_outer_percentiles(id)),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),

        tabPanel(
          title = "Plot - Statistics",
          conditionalPanel(
            "input.type == 'Monthly'", ns = NS(id),
            uiOutput(ns("monthy_line_stat"))),
          select_plot_options(
            select_plot_title(id,name = "plot_title_line"),
            select_plot_log(id, value = default("plot_annual_stats",
                                                "log_discharge"),
                            name = "plot_log_line")),
          ggiraph::girafeOutput(ns("plot_line"), height = opts$plot_height)
        ),

        ## Table ---------------------
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

server_annual_stats <- function(id, data_settings, data_raw,
                                data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ------------------
    observe(shinyjs::toggleState("months_plot",
                                 condition = input$type == "Monthly"))

    output$monthy_line_stat <- renderUI({
      selectInput(NS(id, "monthy_line_stat"),
                  label = "Month Statistic to Plot",
                  choices = gsub("\\_.*","",names(plot_monthly_stats(data = data_raw(),
                                                                     percentiles = as.numeric(input$extra_percentiles)))))
    })

    output$test <- renderText({
      input$monthy_line_stat
    })

    # Titles --------------
    titles <- reactive({
      title(data_settings(), glue::glue("{input$type} Statistics"))
    })

    # Plot -----------------------------
    plot <- reactive({
      check_data(data_loaded())

      validate(
        need(length(input$inner_percentiles) %in% c(0, 2) &
               length(input$outer_percentiles) %in% c(0, 2),
             glue::glue("Inner and outer percentiles must each have two ",
                        "(or no) values, corresponding to the limits of the ",
                        "plot ribbons")))

      req(!is.null(input$plot_log_all), input$type)

      data_flow <- data_raw()

      if(input$type == "Monthly") {
        pi <- c("months","log_discharge")
        e <- glue::glue(
          "months = {conseq(input$months_plot)}, log_discharge = {input$plot_log_all}")
      } else {
        pi <- c("log_discharge")
        e <- glue::glue("log_discharge = {input$plot_log_all}")
      }

      g <- switch(input$type,
                  "Monthly" = "plot_monthly_stats2",
                  "Annual" = "plot_annual_stats2") %>%
        create_fun(data_name = "data_flow", input,
                   input_data = data_settings(),
                   params_ignore = pi, extra = e)

      code$plot <- g
      labels$plot <- glue::glue("Plot {input$type} statistics")

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      date_cols <- "Year"
      if(input$type == "Monthly") date_cols <- c(date_cols, "Month")
      stats <- names(g$data) # Get stats from plot data
      #stats <- stats[!stats %in% date_cols] # Omit these

      # Add vline
      g <- g + create_vline_interactive(
        data = g$data, stats, size = dplyr::if_else(input$type == "Annual", 10, 2))

      g
    })

    dims <- reactive({
      if(input$type == "Monthly"){
        c(13, 8) * opts$scale
      } else {
        c(12, 6) * opts$scale
      }
    })

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(), width_svg = dims()[1], height_svg = dims()[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0("annual_stats_", input$type)),
             data_settings, dims())

    titles_line <- reactive({
      if(input$type == "Monthly") {
        req(input$monthy_line_stat)
        title(data_settings(), glue::glue("{input$type} {input$monthy_line_stat}"))
      } else {
        title(data_settings(), glue::glue("{input$type} Statistics"))

      }
    })

    # Plot -----------------------------
    plot_line <- reactive({
      check_data(data_loaded())

      req(!is.null(input$plot_log_line), input$type)


      data_flow <- data_raw()

      if (is.null(input$extra_percentiles)) {
        pi <- c("log_discharge", "percentiles")
        e <- glue::glue("log_discharge = {input$plot_log_line}, percentiles = NA")
      } else {
        pi <- c("log_discharge","percentiles")
        e <- glue::glue("log_discharge = {input$plot_log_line},
                        percentiles = c({glue::glue_collapse(input$extra_percentiles, sep = ', ')})")

      }

      if(input$type == "Monthly") {
        pi <- c(pi, "months")
        e <- glue::glue(e, ", months = {conseq(input$months_plot)}")
      } else {
        pi <- pi
        e <- e
      }

      g <- switch(input$type,
                  "Monthly" = "plot_monthly_stats",
                  "Annual" = "plot_annual_stats") %>%
        create_fun(data_name = "data_flow", input,
                   input_data = data_settings(),
                   params_ignore = pi, extra = e)

      code$plot_line <- g
      labels$plot_line <- glue::glue("Plot {input$type} statistics")

      if (input$type == "Monthly"){
        req(input$monthy_line_stat)
        g <- eval_check(g)[[paste0(input$monthy_line_stat ,"_Monthly_Statistics")]]
      } else {
        g <- eval_check(g)[[1]]
      }

      # Add title
      if(input$plot_title_line) {
        g <- g +
          ggplot2::ggtitle(titles_line()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      date_cols <- "Year"
      if(input$type == "Monthly") date_cols <- c(date_cols, "Month")
      stats <- names(g$data) # Get stats from plot data
      #stats <- stats[!stats %in% date_cols] # Omit these

      # Add vline
      # g <- g + create_vline_interactive(
      #   data = g$data, stats, combine = TRUE, size = dplyr::if_else(input$type == "Annual", 4, 2),
      #   )

        # data = g$data, stats, combine = TRUE, size = dplyr::if_else(input$type == "Annual", 4, 2),



      if (input$type == "Monthly"){
        g <- g + ggiraph::geom_point_interactive(
          mapping = ggplot2::aes(x=.data$Year, y = .data$Value,
                                 tooltip = glue::glue(
                                   "{.data$Month} {.data$Stat2}\n",
                                   "Year: {.data$Year}\n",
                                   "Discharge: {round(.data$Value, 4)}"),
                                 data_id = .data$Year),
          size = 3, alpha = 0.005)
      } else {
        g <- g + ggiraph::geom_point_interactive(
          mapping = ggplot2::aes(x=.data$Year, y = .data$Value,
                                 tooltip = glue::glue(
                                   "{.data$Statistic}\n",
                                   "Year: {.data$Year}\n",
                                   "Discharge: {round(.data$Value, 4)}"),
                                 data_id = .data$Year),
          size = 3, alpha = 0.005)
      }

      g
    })

    dims_line <- reactive({
      if(input$type == "Monthly"){
        c(13, 8) * opts$scale
      } else {
        c(12, 6) * opts$scale
      }
    })

    output$plot_line <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_line(), width_svg = dims_line()[1], height_svg = dims_line()[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot_line", plot = plot_line,
             name = reactive(paste0("annual_stats_", input$type)),
             data_settings, dims_line())

    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$type)

      data_flow <- data_raw()

      perc <- c(#input$inner_percentiles,
        #input$outer_percentiles,
        input$extra_percentiles) %>%
        unique() %>%
        as.numeric() %>%
        sort()

      #
      # pi <- "percentiles"
      # e <- glue::glue("percentiles = c({glue::glue_collapse(perc, sep = ', ')})")

      if (is.null(input$extra_percentiles)) {
        pi <- "percentiles"
        e <- glue::glue("percentiles = NA")
      } else {
        pi <- "percentiles"
        e <- glue::glue("percentiles = c({glue::glue_collapse(input$extra_percentiles, sep = ', ')})")
      }

      if(input$type == "Monthly") {
        pi <- c(pi, "months")
        e <- glue::glue(
          "{e}, months = c({glue::glue_collapse(input$months_plot, sep = ', ')})")
      }

      t <- switch(input$type,
                  "Monthly" = "calc_monthly_stats",
                  "Annual" = "calc_annual_stats") %>%
        create_fun(data_name = "data_flow", input, input_data = data_settings(),
                   params_ignore = pi, extra = e)

      code$table <- t
      labels$table <- glue::glue("Calculate {input$type} statistics")

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
