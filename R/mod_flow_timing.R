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

# Flow timing ------------------------------
ui_flow_timing <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Timing of Flows"),
      box(
        width = 3,
        helpText("Explore the timing (day of year and date) of portions of total ",
                 "annual flows. For each year, total annual volume is divided into the ",
                 "provided proportions (i.e. 50%) to determine the date of when that ",
                 "proportion occurred (i.e. date when 50% of total flow has occurred). ",
                 "Timing of 50% flow is also called timing of half flow or timing ",
                 "of center of mass (or centroid). ",
                 "The percents/proportions of total annual flows can be modified below."),
        hr(),
        div(style = "max-width: 300px;",
            selectizeInput(ns("percent"),
                           label = "% of Total Annual Flows",
                           choices = c(0:100),
                           selected = c(25, 33, 50, 75),
                           multiple = TRUE),
            bsTooltip(ns("percent"), tips$percent, placement = "left")),
        div(align = "left",uiOutput(ns("ui_year_to_plot")),
            bsTooltip(ns("ui_year_to_plot"), "Specfic year to plot", placement = "left")),
        hr(),
        ui_download(id = ns("plot")), br(),
        helpText("Note: only years of complete data will be used for analysis.")#,
        # textOutput(ns("testings"))
      ),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          select_plot_options(select_plot_title(id)),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height),
          br(),
          ggiraph::girafeOutput(ns("plot_year"), height = opts$plot_height)
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

server_flow_timing <- function(id, data_settings, data_raw,
                               data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # Titles -----------
    titles <- reactive(title(data_settings(), "Flow Timing"))
    titles_year <- reactive(title(data_settings(),
                                  paste0("Flow Timing for ",
                                         ifelse(data_settings()$water_year ==1, "", "Water Year "),
                                         input$year_to_plot)))
    # UI Elements ------------
    complete_years <- reactive({
      y <- get_complete_years_vars(data_raw(),
                                   as.numeric(data_settings()$water_year),
                                   data_settings()$months)
      y <- y[["complete_years"]]
      y <- y[y >= min(data_settings()$years_range)]
      y <- y[y <= max(data_settings()$years_range)]
      y <- y[!y %in% data_settings()$years_exclude]
      y
    })
    # output$ui_year_to_plot <- renderUI({
    #   req(data_settings()$years_range)
    #   select_year_to_plot(id, min(data_settings()$years_range):max(data_settings()$years_range))
    # })
    output$ui_year_to_plot <- renderUI({
      select_year_to_plot(id, complete_years())
    })

    # Preserve dynamic UI inputs during bookmarking
    keep <- c("year_to_plot")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Plot --------------------
    plot <- reactive({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_flow_timing", data_name = "data_flow",
        input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot Annual flow timings"

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
            "{.data$Statistic}\n",
            "Year: {.data$Year}\n",
            "Day of Year: {.data$Value}\n",
            "Date: {yday_as_date(.data$Value, .data$Year, data_settings()$water_year)}"),
            data_id = .data$Year), size = 3)
    })


    dims <- c(12, 7) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts(selection = "single"))
    })

    # Observe clicking event -----------------
    observeEvent(input$plot_selected,{
      updateSelectInput(session = session,
                        inputId = "year_to_plot",
                        selected = as.numeric(input$plot_selected))
    })



    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "flow_timing",
             data_settings, dims)

    # output$plot <- ggiraph::renderGirafe({
    #   ggiraph::girafe(ggobj = plot(),
    #                   width_svg = dims[1],
    #                   height_svg = dims[2],
    #                   options = ggiraph_opts())
    # })


    # Plot Year --------------------
    plot_year <- reactive({
      check_data(data_loaded())
      req(input$percent,
          input$year_to_plot)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_flow_timing_year", data_name = "data_flow",
        input, input_data = data_settings(),
        extra = glue::glue("year_to_plot = {input$year_to_plot}"))

      code$plot_year <- g
      labels$plot_year <- "Plot Annual flow timings YEAR"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles_year()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            x = .data$Date, y = .data$Value2, colour = .data$Percent,
            tooltip = glue::glue("{.data$Percent} Total Flow\n",
                                 "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$DayofYear}\n",
                                 "Date: {.data$Flow_Date}"),
            data_id = .data$DayofYear),
          size = 4, na.rm = TRUE)
    })

    dims2 <- c(13, 6) * opts$scale

    output$plot_year <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_year(),
                      width_svg = dims2[1],
                      height_svg = dims2[2],
                      options = ggiraph_opts())
    })

    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_annual_flow_timing", data_name = "data_flow",
        input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate Annual flow timings"

      eval_check(t) %>%
        prep_DT()
    })

    output$table_title <- renderText(titles())



    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(
      code, labels, data_code,
      order = c("data_raw", "plot", "plot_year", "table")))

  })
}
