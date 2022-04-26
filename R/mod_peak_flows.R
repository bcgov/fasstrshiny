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

# Peak flows ------------------------
ui_peak_flows <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Maximum and Minimum Flows"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),hr(),
        uiOutput(ns("ui_display")),
        div(align = "left",uiOutput(ns("ui_year_to_plot")),
            bsTooltip(ns("ui_year_to_plot"), "Specfic year to plot", placement = "left")),
        hr(),

        h4("Low Flow Options"),
        fluidRow(column(width = 5,
                        div(align = "left",uiOutput(ns("roll_days_low")))),
                 column(width = 7,
                        div(align = "left",uiOutput(ns("months_low"))))),
        h4("High Flow Options"),
        fluidRow(column(width = 5,
                        div(align = "left",uiOutput(ns("roll_days_high")))),
                 column(width = 7,
                        div(align = "left",uiOutput(ns("months_high"))))),
        hr()
      ),
      tabBox(
        width = 9,

        ### Table ---------------------
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

server_peak_flows <- function(id, data_settings, data_raw,
                              data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {


    # Titles --------------------
    titles <- reactive(title(data_settings(), "Annual Peaks"))

    # UI Elements ------------
    # Plot display
    output$ui_display <- renderUI({
      req(plots())
      select_plot_display(id, plots())
    })
    output$ui_year_to_plot <- renderUI({
      req(data_settings()$years_range)
      select_year_to_plot(id, data_settings()$years_range)
    })
    # Observe clicking event -----------------
    observeEvent(input$plot_selected,{
      updateSelectInput(session = session,
                        inputId = "year_to_plot",
                        selected = as.numeric(input$plot_selected))
    })
    output$roll_days_low <- renderUI({
      req(data_settings()$roll_days)
      tagList(
        numericInput(NS(id, "roll_days_low"),
                     label = "Rolling Average Days",
                     value = data_settings()$roll_days,
                     min = 1, max = 100)
      )
    })
    output$roll_days_high <- renderUI({
      req(data_settings()$roll_days)
      tagList(
        numericInput(NS(id, "roll_days_high"),
                     label = "Rolling Average Days",
                     value = data_settings()$roll_days,
                     min = 1, max = 100)
      )
    })
    output$months_low <- renderUI({
      req(data_settings()$months,
          data_settings()$water_year)

      # Arrange months by water year
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(data_settings()$water_year)],
             m[m < as.numeric(data_settings()$water_year)])

      tagList(
        selectizeInput(NS(id, "months_low"),
                       label = "Months",
                       choices = m,
                       selected = as.numeric(data_settings()$months),
                       multiple = TRUE)#,
        # bsTooltip("months_low"), "Specfic year to plot", placement = "left")
      )
    })
    output$months_high <- renderUI({
      req(data_settings()$months,
          data_settings()$water_year)

      # Arrange months by water year
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(data_settings()$water_year)],
             m[m < as.numeric(data_settings()$water_year)])

      tagList(
        selectizeInput(NS(id, "months_high"),
                       label = "Months",
                       choices = m,
                       selected = as.numeric(data_settings()$months),
                       multiple = TRUE)#,
        # bsTooltip("months_low"), "Specfic year to plot", placement = "left")
      )
    })

    # Plot --------------------
    plots <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      p <- c(glue::glue("roll_days_low = {input$roll_days_low}"),
             glue::glue("roll_days_high = {input$roll_days_high}"),
             glue::glue("months_low = {conseq(input$months_low)}"),
             glue::glue("months_high = {conseq(input$months_high)}")
      ) %>%
        glue::glue_collapse(sep = ", ")

      g <- create_fun(fun = "plot_annual_peaks", data_name = "data_flow",
                      input,
                      input_data = data_settings(),
                      params_ignore = c("roll_days","months"),
                      extra = p)

      g <- eval_check(g)

      # Add interactivity
      d1 <- dplyr::left_join(g[[1]]$data, g[[2]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_doy")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year, data_settings()$water_year))

      g[["Annual_Peak_Flows"]]$data <- d1
      g[["Annual_Peak_Flows"]] <- g[["Annual_Peak_Flows"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "{.data$Statistic}\n",
            "Year: {.data$Year}\n",
            "Date: {.data$Date}\n",
            "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$Value_doy}\n",
            "Discharge: {round(.data$Value, 4)}"),
            data_id = .data$Year), size = 3)

      d2 <- dplyr::left_join(g[[2]]$data, g[[1]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_discharge")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year, data_settings()$water_year))

      g[["Annual_Peak_Flows_Dates"]]$data <- d2
      g[["Annual_Peak_Flows_Dates"]] <- g[["Annual_Peak_Flows_Dates"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "{.data$Statistic}\n",
            "Year: {.data$Year}\n",
            "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$Value}\n",
            "Date: {.data$Date}\n",
            "Discharge: {round(.data$Value_discharge, 4)}"),
            data_id = .data$Year), size = 3)

      # Add title
      if(input$plot_title) {
        for(i in seq_len(length(g))) {
          g[[i]] <- g[[i]] +
            ggplot2::ggtitle(title(
              data_settings(), stringr::str_replace_all(names(g)[i], "_", " "))) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = 12))
        }
      }

      g
    })

    plot <- reactive({
      req(input$display, input$display %in% names(plots()))
      plots()[[input$display]]
    })

    dims <- c(12, 8) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts(selection = "single"))
    })


    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0("peak_flows_", input$display)),
             data_settings, dims)

    # Plot --------------------
    plot_year <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      p <- c(glue::glue("roll_days_low = {input$roll_days_low}"),
             glue::glue("roll_days_high = {input$roll_days_high}"),
             glue::glue("months_low = {conseq(input$months_low)}"),
             glue::glue("months_high = {conseq(input$months_high)}"),
             glue::glue("year_to_plot = {input$year_to_plot}")
      ) %>%
        glue::glue_collapse(sep = ", ")

      g <- create_fun(fun = "plot_annual_peaks_year", data_name = "data_flow",
                      input,
                      input_data = data_settings(),
                      params_ignore = c("roll_days","months"),
                      extra = p)

      code$plot <- g
      labels$plot <- "Plot Peaks YEAR"

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

      data_flow <- data_raw()

      p <- c(glue::glue("roll_days_low = {input$roll_days_low}"),
             glue::glue("roll_days_high = {input$roll_days_high}"),
             glue::glue("months_low = {conseq(input$months_low)}"),
             glue::glue("months_high = {conseq(input$months_high)}")
      ) %>%
        glue::glue_collapse(sep = ", ")

      t <- create_fun(fun = "calc_annual_peaks", data_name = "data_flow",
                      input,
                      input_data = data_settings(),
                      params_ignore = c("roll_days","months"),
                      extra = p)

      code$table <- t
      labels$table <- "Calculate Annual Peak flows"

      eval_check(t) %>%
        prep_DT()
    })

    output$table_title <- renderText(title(data_settings(), "Peak Flows"))


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))
  })
}
