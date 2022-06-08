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


ui_flows <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Flow Duration and Percentiles"),

      ## Settings -----------------------------
      box(
        width = 3,
        helpText("Explore flow duration curves and flow percentiles. ",
                 "A flow duration curve is a cumulative frequency curve that ",
                 "shows the percent of time specific flows are equaled or exceeded, ",
                 "and may be used to predict distribution of future flows. Percentiles ",
                 "similarly indicate the percent of a distribution that is equal ",
                 "to or below it. The time periods to summarize ",
                 "and adding a custom month period can be modified below."),
        helpText("Click 'Compute' after making any changes to settings."),
        # Compute button
        bsButton(ns("compute"), "Compute", style = "primary",
                 class = "centreButton"),
        hr(),
        h5("Select Time Periods"), #, style = "margin-left: 15px;"
        uiOutput(ns("ui_months_plot")),
        div(id = ns("longterm_tip"),
            prettySwitch(ns("longterm"),
                         label = "Include Long-term",
                         value = TRUE,
                         status = "success", slim = TRUE)),
        bsTooltip(ns("longterm_tip"), tips$longterm, placement = "left"),
        hr(),
        h5("Add Custom Month Period"), #, style = "margin-left: 15px;"
        helpText("Select months to combine and provide a label to name ",
                 "the time period."),
        select_custom_months(id),
        hr(),
        ui_download(id = ns("plot"))
      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot - Flow Duration",
          select_plot_options(
            select_plot_title(id),
            select_plot_log(id, value = default("plot_flow_duration",
                                                "log_discharge"))),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput(ns("plot"), height = opts$plot_height))
        ),

        ## Table ---------------------
        tabPanel(
          title = "Table - Percentiles",
          h4(textOutput(ns("table_title"))),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table")))
        ),

        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_flows <- function(id, data_settings, data_raw,
                         data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements -----------------------------------------------

    output$ui_months_plot <- renderUI({
      tagList(
        checkboxGroupButtons(
          NS(id, "months_plot"),
          label = "Months:",
          choices = list("Jan" = 1, "Feb" = 2,
                         "Mar" = 3, "Apr" = 4,
                         "May" = 5, "Jun" = 6,
                         "Jul" = 7, "Aug" = 8,
                         "Sep" = 9, "Oct" = 10,
                         "Nov" = 11, "Dec" = 12),
          selected = data_settings()$months),
        bsTooltip(NS(id, "months_plot"),
                  "Months to include/exclude from the plot",
                  placement = "left"))
    })


    # Preserve dynamic UI inputs during bookmarking
    keep <- c("months_plot")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Change button status -----------------------

    # Current settings
    settings_current <- reactive({
      s <- get_inputs(input, which = c(
        "months_plot",
        "plot_title", "plot_log", "custom_months", "custom_months_label",
        "longterm"))
      s$data_raw <- data_raw()
      s$data_settings <- data_settings()
      s
    })

    # Settings at last Compute
    settings_last <- reactive(settings_current()) %>% bindEvent(input$compute)

    observe({
      settings_current()
      # Change buttons and record status if changes
      if(input$compute > 0) {
        update_on_change(session, id,
                         current = settings_current(), last = settings_last(),
                         labels = paste0("Compute<br><small>",
                                         c("Settings/Data have changed",
                                           "No changes since last computation"),
                                         "</small>"))
      }
    })



    # Titles ----------------
    titles <- reactive(title(data_settings(), "Flow Duration"))

    # Plot --------------------

    plot <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      if(is.null(input$months_plot)) {
        mp <- "NULL"
      } else mp <- conseq(input$months_plot)

      g <- create_fun(fun = "plot_flow_duration", data_name = "data_flow",
                      input, input_data = data_settings(),
                      params_ignore = "months",
                      extra = glue::glue("months = {mp}"))

      code$plot <- g
      labels$plot <- "Plot flow duration"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "Month: {.data$Month}\n",
            "Percent Time: {.data$Percentile}% time\n",
            "Discharge: {round(.data$Value, 3)}",
            .trim = FALSE),
            data_id = .data$Percentile),
          show.legend = FALSE, alpha = 0.01, size = 3)
    }) %>%
      bindEvent(input$compute)


    dims <- c(12, 6) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "flow_duration",
             data_settings, dims)



    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      if(is.null(input$months_plot)) {
        mp <- "NULL"
      } else mp <- conseq(input$months_plot)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_longterm_daily_stats",
        data_name = "data_flow", input, input_data = data_settings(),
        params_ignore = "months",
        extra = glue::glue("percentiles = 0:100, months = {mp}"),
        end = "%>% dplyr::select(-Mean, -Median, -Minimum, -Maximum)")

      code$table <- t
      labels$table <- "Calculate flows duration"

      eval_check(t) %>%
        tidyr::pivot_longer(cols = -dplyr::any_of(c("STATION_NUMBER", "Month")),
                            names_to = "Percentiles", values_to = "value") %>%
        tidyr::pivot_wider(names_from = .data$Month, values_from = .data$value) %>%
        prep_DT()
    }) %>%
      bindEvent(input$compute)

    output$table_title <- renderText(titles())

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
