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


# Normal Days ------------------------
ui_normal_days <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Number of Days Normal, and Above and Below Normal"),
      box(
        width = 3,
        helpText("Explore the number of days per year normal, and above and below",
                 "the 'normal' ",
                 "range (typically between 25 and 75th percentiles, set below) for ",
                 "each day of the year. Upper and lower range percentiles are ",
                 "calculated for each day of the year from all years, and then ",
                 "each daily flow value for each year is compared."),
        helpText("To view the normal category days for a specific year on the bottom plot, ",
                 "choose the year below or click on a year in the top plot."),
        hr(),
        sliderInput(ns("normal_percentiles"), label = "Percentiles Normal Range",
                    value = c(25, 75), min = 0, max = 100, step = 1),
        bsTooltip(ns("normal_percentiles"), tips$normal_percentiles, placement = "left"),
        hr(),
        h4("Year Plot Options"),
        div(align = "left",uiOutput(ns("ui_year_to_plot")),
            bsTooltip(ns("ui_year_to_plot"), "Specfic year to plot", placement = "left")),

        hr(),
        fluidRow(
          column(width = 6, ui_download(id = ns("plot"), name = "Download All Years Plot")),
          column(width = 6, ui_download(id = ns("plot_year"), name = "Download Year Plot"))
        ), br(),
        helpText("Note: Analysis methodology is ",
                 "based on Environment and Climate Change Canada's ",
                 a(href = paste0("https://www.canada.ca/en/environment-climate-change",
                                 "/services/environmental-indicators/water-quantity-",
                                 "canadian-rivers.html"),
                   "Canadian Environmental Sustainability Indicator Water Quantity Indicator"),"."),
        helpText("Only years of complete data will be used for analysis.")
      ),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          select_plot_options(select_plot_title(id)),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height),
          br(),
          select_plot_options(
            select_plot_title(id, name = "plot_title_year"),
            select_plot_log(id, value = TRUE),
            prettySwitch(ns("plot_normal_percentiles"),
                         label = "Plot Normal Percentiles",
                         value = TRUE, status = "success", slim = TRUE)),
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

server_normal_days <- function(id, data_settings, data_raw,
                               data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # Titles --------------------
    titles <- reactive(title(data_settings(), "Normal Days"))
    titles_year <- reactive(title(data_settings(),
                                  paste0("Normal Days for ",
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
      req(input$normal_percentiles)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_normal_days", data_name = "data_flow",
        input, input_data = data_settings())

      code$plot <- g
      labels$plot <- "Plot annual number of days within, above and below normal"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = 12))
      }

      g$data <- dplyr::left_join(g$data, g$data %>%
                                   dplyr::mutate(tooltip = glue::glue(
                                     "{Statistic}: {Value}")) %>%
                                   dplyr::group_by(.data$Year) %>%
                                   dplyr::summarize(tooltip = glue::glue(
                                     "Year: {.data$Year}\n",
                                     glue::glue_collapse(.data$tooltip, "\n"))) %>%
                                   dplyr::distinct(), by = "Year")

      # Add interactivity
      g <- g + ggiraph::geom_bar_interactive(
        position = "stack", stat = "identity", alpha = 0.005,
        ggplot2::aes(tooltip = tooltip,
                     data_id = .data$Year), size = 3)
    })

    dims <- c(12, 6) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts(selection = "single"))
    })


    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "normal_days", data_settings, dims)

    # Observe clicking event -----------------
    observeEvent(input$plot_selected,{
      updateSelectInput(session = session,
                        inputId = "year_to_plot",
                        selected = as.numeric(input$plot_selected))
    })


    # Plot --------------------
    plot_year <- reactive({
      check_data(data_loaded())
      req(input$normal_percentiles,
          input$year_to_plot,
          !is.null(input$plot_log))

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_annual_normal_days_year", data_name = "data_flow",
        input, input_data = data_settings(),
        extra = glue::glue("year_to_plot = {input$year_to_plot},
                      plot_normal_percentiles = {input$plot_normal_percentiles}"))

      code$plot_year <- g
      labels$plot_year <- "Plot Annual Normal Days YEAR"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title_year) {
        g <- g +
          ggplot2::ggtitle(titles_year()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      # Add interactivity
      g <- g +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            x = .data$Date, y = .data$Value,
            tooltip = glue::glue("{.data$Normal}\n",
                                 "Day of {ifelse(data_settings()$water_year==1,'Year', 'Water Year')}: {.data$DayofYear}\n",
                                 "Date: {.data$Flow_Date}\n",
                                 "Discharge: {round(.data$Value,4)}"),
            data_id = .data$DayofYear),
          size = 4,  na.rm = TRUE, alpha = 0.01)

    })

    dims2 <- c(13, 6) * opts$scale


    output$plot_year <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_year(),
                      width_svg = dims2[1],
                      height_svg = dims2[2],
                      options = ggiraph_opts())
    })

    download(id = "plot_year", plot = plot_year,
             name = reactive(paste0("normal_days_", input$year_to_plot)),
             data_settings, dims)

    # Table -----------------------
    output$table <- DT::renderDT({
      req(input$normal_percentiles)

      data_flow <- data_raw()

      t <- create_fun(
        fun = "calc_annual_normal_days", data_name = "data_flow",
        input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate annual number of days within, above and below normal"

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
