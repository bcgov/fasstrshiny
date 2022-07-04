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
ui_annual_extremes <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Maximum and Minimum Flows"),
      box(
        width = 3,
        helpText("Explore annual extreme (maximum and minimum) flows and their timing. ",
                 "The duration of rolling day averages for each extreme, the months ",
                 "to limit each extreme, and the type of plot (top plot) to display can be modifed below. "),
        helpText("To view the values and timing for a specific year on the bottom plot, ",
                 "choose the year below or click on a year value in the top plot. ",
                 "Note: only years of complete data are used to compute percentile ribbons."),hr(),
        uiOutput(ns("ui_display")),
        hr(),
        h4("Maximum Flow Options"),
        # fluidRow(column(width = 5,
        #                 div(align = "left",uiOutput(ns("roll_days_max")))),
        #          column(width = 7,
        #                 div(align = "left",uiOutput(ns("months_max"))))),
        fluidRow(column(width = 6, div(align = "left",uiOutput(ns("roll_days_max"))))),
        div(align = "left",uiOutput(ns("months_max"))),
        h4("Minimum Flow Options"),
        # fluidRow(column(width = 5,
        #                 div(align = "left",uiOutput(ns("roll_days_min")))),
        #          column(width = 7,
        #                 div(align = "left",uiOutput(ns("months_min"))))),
        fluidRow(column(width = 6, div(align = "left",uiOutput(ns("roll_days_min"))))),
        div(align = "left",uiOutput(ns("months_min"))),
        hr(),
        h4("Year Plot Options"),
        div(align = "left",uiOutput(ns("ui_year_to_plot")),
            bsTooltip(ns("ui_year_to_plot"), "Specfic year to plot", placement = "left")),
        hr(),
        fluidRow(
          column(width = 6, ui_download(id = ns("plot"), name = "Download All Years Plot")),
          column(width = 6, ui_download(id = ns("plot_year"), name = "Download Year Plot"))
        )
      ),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          select_plot_options(select_plot_title(id)),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height),
          select_plot_options(
            select_plot_title(id, name = "plot_title_year"),
            select_plot_log(id, value = TRUE),
            prettySwitch(ns("plot_max"),
                         label = "Plot Maximum",
                         value = TRUE, status = "success", slim = TRUE),
            prettySwitch(ns("plot_min"),
                         label = "Plot Minimum",
                         value = TRUE, status = "success", slim = TRUE),
            prettySwitch(ns("plot_normal_percentiles"),
                         label = "Plot Normal Percentiles",
                         value = TRUE, status = "success", slim = TRUE),
            sliderInput(ns("normal_percentiles"), label = "Percentiles Normal Range:",
                        value = c(25, 75), min = 0, max = 100, step = 1)),
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

server_annual_extremes <- function(id, data_settings, data_raw,
                                   data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {


    # Titles --------------------
    titles <- reactive(title(data_settings(), "Annual Extremes"))
    titles_year <- reactive(title(data_settings(),
                                  paste0("Annual Extremes for ",
                                         ifelse(data_settings()$water_year ==1, "", "Water Year "),
                                         input$year_to_plot)))
    # UI Elements ------------
    # Plot display
    output$ui_display <- renderUI({
      req(plots())
      select_plot_display(id, plots(), label = "All Years Display Plot:")
    })

    output$ui_year_to_plot <- renderUI({
      req(data_settings()$years_range)
      select_year_to_plot(id, data_settings()$years_range[1]:data_settings()$years_range[2])
    })

    # Observe clicking event -----------------
    observeEvent(input$plot_selected,{
      updateSelectInput(session = session,
                        inputId = "year_to_plot",
                        selected = as.numeric(input$plot_selected))
    })
    output$roll_days_min <- renderUI({
      req(data_settings()$roll_days)
      tagList(
        numericInput(NS(id, "roll_days_min"),
                     label = "Rolling Average Days:",
                     value = data_settings()$roll_days,
                     min = 1, max = 100)
      )
    })
    output$roll_days_max <- renderUI({
      req(isolate(data_settings()$roll_days))
      tagList(
        numericInput(NS(id, "roll_days_max"),
                     label = "Rolling Average Days:",
                     value = isolate(data_settings()$roll_days),
                     min = 1, max = 100)
      )
    })
    output$months_min <- renderUI({

      req(data_settings()$months,
          data_settings()$water_year)

      # Arrange months by water year
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(data_settings()$water_year)],
             m[m < as.numeric(data_settings()$water_year)])

      # tagList(
      #   sliderTextInput(NS(id, "months_min"),
      #                  label = "Months",
      #                  choices = m,
      #                  selected = as.numeric(data_settings()$months),
      #                  multiple = TRUE)#,
      #   # bsTooltip("months_min"), "Specfic year to plot", placement = "left")
      # )

      tagList(
        sliderTextInput(
          inputId = NS(id, "months_min"),
          label = "Months:",
          choices = month.abb[m],
          selected = c(month.abb[as.numeric(data_settings()$months)[1]],
                       month.abb[as.numeric(data_settings()$months)[length(data_settings()$months)]]),
          grid = TRUE,
          hide_min_max = TRUE
        )
      )
    })
    months_min <- reactive({
      req(input$months_min)
      mn <- c(stats::setNames(1:12, month.abb),stats::setNames(1:12, month.abb))
      mn_list <- mn[which(names(mn) == input$months_min[1], mn)[1]:length(mn)]
      mn_list <- mn_list[1:which(names(mn_list) == input$months_min[2], mn_list)[1]]
      mn_list
    })
    months_max <- reactive({
      req(input$months_max)
      mn <- c(stats::setNames(1:12, month.abb),stats::setNames(1:12, month.abb))
      mn_list <- mn[which(names(mn) == input$months_max[1], mn)[1]:length(mn)]
      mn_list <- mn_list[1:which(names(mn_list) == input$months_max[2], mn_list)[1]]
      mn_list
    })

    output$months_max <- renderUI({
      req(data_settings()$months,
          data_settings()$water_year)

      # Arrange months by water year
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(data_settings()$water_year)],
             m[m < as.numeric(data_settings()$water_year)])

      # tagList(
      #   selectizeInput(NS(id, "months_max"),
      #                  label = "Months",
      #                  choices = m,
      #                  selected = as.numeric(data_settings()$months),
      #                  multiple = TRUE)#,
      #   # bsTooltip("months_min"), "Specfic year to plot", placement = "left")
      # )
      tagList(
        sliderTextInput(
          inputId = NS(id, "months_max"),
          label = "Months:",
          choices = month.abb[m],
          selected = c(month.abb[as.numeric(data_settings()$months)[1]],
                       month.abb[as.numeric(data_settings()$months)[length(data_settings()$months)]]),
          grid = TRUE,
          hide_min_max = TRUE
        )
      )
    })

    # Preserve dynamic UI inputs during bookmarking
    keep <- c("year_to_plot", "roll_days_min", "roll_days_max", "months_min",
              "months_max")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # observe({
    #   input$plot_normal_percentiles
    # })
    # Plot --------------------
    plots <- reactive({
      check_data(data_loaded())
      req(input$roll_days_min,input$roll_days_max)

      data_flow <- data_raw()

      p <- c(glue::glue("roll_days_min = {input$roll_days_min}"),
             glue::glue("roll_days_max = {input$roll_days_max}"),
             glue::glue("months_min = {conseq(months_min())}"),
             glue::glue("months_max = {conseq(months_max())}")
      ) %>%
        glue::glue_collapse(sep = ", ")

      g <- create_fun(fun = "plot_annual_extremes", data_name = "data_flow",
                      input,
                      input_data = data_settings(),
                      params_ignore = c("roll_days","months"),
                      extra = p)

      code$plot <- g
      labels$plot <- "Plot Annual Extremes"

      g <- eval_check(g)

      # Add interactivity
      d1 <- dplyr::left_join(g[[1]]$data, g[[2]]$data,
                             by = c("Year", "Statistic"),
                             suffix = c("", "_doy")) %>%
        dplyr::mutate(Date = yday_as_date(.data$Value_doy, .data$Year, data_settings()$water_year))

      g[["Annual_Extreme_Flows"]]$data <- d1
      g[["Annual_Extreme_Flows"]] <- g[["Annual_Extreme_Flows"]] +
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

      g[["Annual_Extreme_Flows_Dates"]]$data <- d2
      g[["Annual_Extreme_Flows_Dates"]] <- g[["Annual_Extreme_Flows_Dates"]] +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = glue::glue(
            "{.data$Statistic}\n",
            "Year: {.data$Year}\n",
            "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$Value}\n",
            "Date: {.data$Date}\n",
            "Discharge: {round(.data$Value_discharge, 4)}"),
            data_id = .data$Year), size = 3, alpha = 0.005, fill = "white")

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

    dims <- c(12, 6) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts(selection = "single"))
    })


    # Download Plot -----------------
    download(id = "plot", plot = plot,
             name = reactive(paste0(input$display)),
             data_settings, dims)

    # Plot Year --------------------
    plot_year <- reactive({
      check_data(data_loaded())
      req(input$roll_days_min,input$roll_days_max,
          input$year_to_plot,
          input$normal_percentiles#,
          #  input$plot_normal_percentiles,
        #  input$plot_max,
         # input$plot_min
      )

      data_flow <- data_raw()

      p <- glue::glue("roll_days_min = {input$roll_days_min},
                      roll_days_max = {input$roll_days_max},
                      months_min = {conseq(months_min())},
                      months_max = {conseq(months_max())},
                      year_to_plot = {input$year_to_plot},
                      plot_normal_percentiles = {input$plot_normal_percentiles},
                      plot_max = {input$plot_max},
                      plot_min = {input$plot_min}")

      g <- create_fun(fun = "plot_annual_extremes_year", data_name = "data_flow",
                      input,
                      input_data = data_settings(),
                      params_ignore = c("roll_days","months"),
                      extra = p)

      code$plot_year <- g
      labels$plot_year <- "Plot Annual Extremes by Year"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title_year) {
        g <- g +
          ggplot2::ggtitle(titles_year()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g +
        ggiraph::geom_rect_interactive(
          ggplot2::aes(
            xmin = .data$Min_Start, xmax = .data$Min_End, ymax=Inf, ymin=0,
            tooltip = glue::glue("{input$roll_days_min}-Day Minimum\n",
                                 "Discharge: {round(.data$Min_Value,4)}\n",
                                 "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$DayofYear}\n",
                                 "Date: {.data$Flow_Date}"),
            data_id = .data$DayofYear),
          na.rm = TRUE, alpha = 0.005, fill = "white")+
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            x = .data$AnalysisDate, y = Min_Value,
            tooltip = glue::glue("{input$roll_days_min}-Day Minimum\n",
                                 "Discharge: {round(.data$Min_Value,4)}\n",
                                 "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$DayofYear}\n",
                                 "Date: {.data$Flow_Date}"),
            data_id = .data$DayofYear),
          na.rm = TRUE, alpha = 0.005, fill = "white")+
        ggiraph::geom_rect_interactive(
          ggplot2::aes(
            xmin = .data$Max_Start, xmax = .data$Max_End, ymax=Inf, ymin=0,
            tooltip = glue::glue("{input$roll_days_max}-Day Maximum\n",
                                 "Discharge: {round(.data$Max_Value,4)}\n",
                                 "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$DayofYear}\n",
                                 "Date: {.data$Flow_Date}"),
            data_id = .data$DayofYear),
          na.rm = TRUE, alpha = 0.005, fill = "white")+
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            x = .data$AnalysisDate, y = Max_Value,
            tooltip = glue::glue("{input$roll_days_max}-Day Maximum\n",
                                 "Discharge: {round(.data$Max_Value,4)}\n",
                                 "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$DayofYear}\n",
                                 "Date: {.data$Flow_Date}"),
            data_id = .data$DayofYear),
          na.rm = TRUE, alpha = 0.005, fill = "white")
    })

    dims2 <- c(13, 6) * opts$scale


    output$plot_year <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_year(),
                      width_svg = dims2[1],
                      height_svg = dims2[2],
                      options = ggiraph_opts())
    })

    download(id = "plot_year", plot = plot_year,
             name = reactive(paste0("extreme_flows_", input$year_to_plot)),
             data_settings, dims)

    # Table -----------------------
    output$table <- DT::renderDT(server = FALSE, {

      check_data(data_loaded())

      data_flow <- data_raw()

      p <- c(glue::glue("roll_days_min = {input$roll_days_min}"),
             glue::glue("roll_days_max = {input$roll_days_max}"),
             glue::glue("months_min = {conseq(months_min())}"),
             glue::glue("months_max = {conseq(months_max())}")
      ) %>%
        glue::glue_collapse(sep = ", ")

      t <- create_fun(fun = "calc_annual_extremes", data_name = "data_flow",
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
    output$code <- renderText(code_format(
      code, labels, data_code,
      order = c("data_raw", "plot", "plot_year", "table")))
  })
}
