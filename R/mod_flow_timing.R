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

        # div(align = "left",
        #     awesomeRadio(ns("plot_type"),
        #                  label = "Plot Type",
        #                  choices = list("All Annual Values", "Selected Year"),
        #                  selected = "All Annual",
        #                  status = "primary")),
        # bsTooltip(ns("plot_type"), "Type of plot to show", placement = "left"),
        div(align = "left",
            selectInput(ns("year_to_plot"),
                        label = "Selected Year to Plot",
                        choices = 1999:2010,
                        selected = 1999)),
        bsTooltip(ns("year_to_plot"), "Specfic year to plot", placement = "left"),

        hr(),
        ui_download(id = ns("plot")), br(),
        helpText("Note: only years of complete data will be used for analysis.")
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
            "Year: {.data$Year}\n",
            "{.data$Statistic}\n",
            "Date: {yday_as_date(.data$Value, .data$Year)}"),
            data_id = .data$Year), size = 3)
    })


    dims <- c(13, 7) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "flow_timing",
             data_settings, dims)

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts())
    })


    # Plot --------------------
    plot_year <- reactive({
      check_data(data_loaded())
      req(input$percent)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_flow_timing_year", data_name = "data_flow",
        input, input_data = data_settings(),
        extra = glue::glue("year_to_plot = '{input$year_to_plot}'"))

      code$plot <- g
      labels$plot <- "Plot Annual flow timings YEAR"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g# +
       # ggiraph::geom_vline_interactive(
      #    xintercept = .$Date, colour = 'grey20', tooltip = .$labs) #+
      # ggplot2::geom_text(data = dts, ggplot2::aes(x = .data$Date,
      #                                             label = .data$labs,
      #                                             hjust = .data$hjust),
      #                     y = Inf, vjust = 2)
    })

    dims2 <- c(13, 7) * opts$scale


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
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
