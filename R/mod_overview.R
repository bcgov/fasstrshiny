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

# Overview ---------------------------------------------------------------
ui_overview <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      h2("Data Overview"),
      # verbatimTextOutput(ns("test")),
      # box(width = 2,
      #     helpText("Explore..."),
      #     hr(),
      #     h3("Options"),
      #     show_ui(ns("show_ltmad"), "Long-term MAD", value = FALSE),
      #     div(id = ns("ltmad_opts"),
      #
      #         selectizeInput(ns("ltmad_percent"),
      #                        label = "Percentages of LTMAD",
      #                        choices = c(1:200, seq(205, 500, 5)),
      #                        selected = c(100,20,10, 5),
      #                        multiple = TRUE,
      #                        options = list(maxItems = 10))),
      #     show_ui(ns("show_quant"), "Frequency Quantile", value = FALSE),
      #     div(id = ns("quant_opts"),
      #         fluidRow(column(width = 6,
      #                         numericInput(ns("duration"), label = "Duration:",value = 30,min = 1, max = 60,step = 1)),
      #                  column(width = 6,
      #                         numericInput(ns("ret_per"), label = "Return Period:",value = 20,min = 1, max = 1000,step = 1))),
      #         uiOutput(ns("ui_freq_months"))),
      #     show_ui(ns("show_annual"), "Annual MAD", value = FALSE),
      #     div(id = ns("annual_opts"),
      #         fluidRow(column(width = 6,
      #                         selectizeInput(ns("ann_ptile"),
      #                                        label = "Percentiles of Annual Means to Plot",
      #                                        choices = 0:100,
      #                                        selected = c(10,90),
      #                                        multiple = TRUE,
      #                                        options = list(maxItems = 2)))))
      # ),
      tabBox(
        width = 12,

        ### HYDAT Info ---------------------
        tabPanel(
          title = "Overview",
          #  box(title = "Monthly Means", status = "primary", solidHeader = TRUE,
          #      ggiraph::girafeOutput(ns("plot_month"), height = opts$plot_height)),

          fluidRow(
            column(width = 4,
                   helpText("Explore various streamflow summary information for your station. ",
                            "View long-term statistics (entire period of record), along with various ",
                            "plots to view monthly and annual hydrologic patterns. ",
                            "Statistcs in the table and plots can me modified below. ",
                            "Can also view the HYDAT summary information for your selected station ",
                            "on the 'HYDAT Information' tab above.",
                            "This page provides just an overview of the data, with the tables, plots ",
                            "and corresponding R code found throughout the app."),
                   hr(),
                   h3("Long-term Data Summary"),
                   DT::dataTableOutput(ns("table3")),
                   hr(),
                   box(
                     title = "Statistics Options", solidHeader = TRUE,status = "primary", width = 9,
                     selectizeInput(ns("ltmad_percent"),
                                    label = "Percentages of LTMAD (Table and Plot):",
                                    choices = c(1:200, seq(205, 500, 5)),
                                    selected = c(100,20,10, 5),
                                    multiple = TRUE,
                                    options = list(maxItems = 10)),
                     selectizeInput(ns("ann_ptile"),
                                    label = "Percentiles of Annual Means (Plot):",
                                    choices = 0:100,
                                    selected = c(10,90),
                                    multiple = TRUE,
                                    options = list(maxItems = 2)),
                     hr(),
                     h5("Low-Flow Probabilites & Quantile (Table and Plot):"),
                     fluidRow(column(width = 6,
                                     numericInput(ns("duration"), label = "Duration:",value = 30,min = 1, max = 60,step = 1)),
                              column(width = 6,
                                     numericInput(ns("ret_per"), label = "Return Period:",value = 20,min = 1, max = 1000,step = 1))),
                     uiOutput(ns("ui_freq_months"))
                   )
                   #  h3("Statistics Options"),


            ),
            column(width = 8,

                   # h3("Monthly Means"),
                   #select_plot_options(),
                   ggiraph::girafeOutput(ns("plot_month"), height = "300px"),#opts$plot_height),
                   br(),
                   # h3("Annual Means"),
                   #select_plot_options(),
                   ggiraph::girafeOutput(ns("plot_annual"), height = "350px"),
                   br(),
                   # select_plot_options( ),
                   ggiraph::girafeOutput(ns("plot_quant"), height = "350px"))
          )
        ),



        #
        #           h3("Percentiles"),
        #
        #           #tableOutput(ns("table")),
        #           # tableOutput(ns("table2")),
        #           #h4(textOutput(ns("quant")))
        #
        #           "n years of data, n missing, start, end, etcs",
        #           "basic month hydrograph - month means, with LTMADs",
        #           "annual daily hydrograph",
        #           "ltmad and 5, 10, 20 percent mads - with options",
        #           "30Q20 and freq - with options for return period/duration (AND MONTHS?)",
        #           "all time low, all time high",
        #           "percentile and rank section? drop box to choose?"

        #,


        ### HYDAT Info ---------------------
        tabPanel(
          title = "HYDAT Information",
          fluidRow(column(width = 5,
                          shinycssloaders::withSpinner(
                            leaflet::leafletOutput(ns("station_map"), width = "100%", height = "500px"))),
                   column(width = 7,
                          h3("HYDAT Information"),
                          DT::dataTableOutput(ns("station_info")))
          ))



      )
    )
  )
}

server_overview <- function(id, data_settings, data_raw,
                            data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # observe(shinyjs::toggle("quant_opts", condition = input$show_quant))
    # observe(shinyjs::toggle("ltmad_opts", condition = input$show_ltmad))
    # observe(shinyjs::toggle("annual_opts", condition = input$show_annual))

    # output$ui_freq_opts <- renderUI({
    #   tagList(
    #     fluidRow(column(width = 6,
    #                     numericInput(NS(id, "duration"), label = "Duration:",value = 30,min = 1, max = 60,step = 1)),
    #              column(width = 6,
    #                     numericInput(NS(id, "ret_per"), label = "Return Period:",value = 20,min = 1, max = 1000,step = 1)))
    #   )
    # })
    output$ui_freq_months <- renderUI({

      # Arrange months by water year
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(data_settings()$water_year)],
             m[m < as.numeric(data_settings()$water_year)])

      tagList(
        sliderTextInput(
          inputId = NS(id, "freq_months"),
          label = "Months:",
          choices = month.abb[m],
          selected = c(month.abb[data_settings()$month[1]],
                       month.abb[data_settings()$month[length(data_settings()$month)]]),
          grid = TRUE,
          hide_min_max = TRUE
        ),
        bsTooltip(NS(id, "freq_months"), tips$months, placement = "left")
      )
    })
    months_all <- reactive({
      req(input$freq_months)
      mn <- c(stats::setNames(1:12, month.abb),stats::setNames(1:12, month.abb))
      mn_list <- mn[which(names(mn) == input$freq_months[1], mn)[1]:length(mn)]
      mn_list <- mn_list[1:which(names(mn_list) == input$freq_months[2], mn_list)[1]]
      mn_list
    })


    # plot_month --------------------
    plot_month <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      if (!is.null(input$ltmad_percent)) {
        ptiles <- input$ltmad_percent
        ptiles <- conseq(ptiles)
      } else {
        ptiles <-  NA
      }

      g <- create_fun(fun = "plot_monthly_means", data_name = "data_flow",
                      input, input_data = data_settings(),
                      params_ignore = c("discharge","percent_MAD"),
                      extra = glue::glue("percent_MAD = {ptiles}")
      )

      code$plot <- g
      labels$plot <- "Plotting monthly means"

      g <- eval_check(g)[[1]]

      # Add title
      g <- g +
        ggplot2::ggtitle("Monthly Means") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.2)))

      # Replace layers with interactive
      g <- g +
        ggiraph::geom_bar_interactive(
          stat = "identity", alpha = 0.005, na.rm = TRUE, width = 0.8,
          ggplot2::aes(data_id = .data$Month,
                       y = Mean,
                       tooltip = glue::glue("Month: {.data$Month}\n",
                                            "Mean: {round(.data$Mean,3)}\n",
                                            "% LTMAD: {round(.data$Mean/.data$LTMAD,3)*100}%\n",
                                            "LTMAD Diff.: {round(.data$Mean - .data$LTMAD,3)}",
                                            .trim = FALSE)
          ))

      if (!is.null(input$ltmad_percent)) {

        g <- g +
          ggiraph::geom_hline_interactive(
            ggplot2::aes(yintercept = Value,
                         tooltip = glue::glue("{.data$LTMAD_Percent}: {round(.data$Value,3)}\n",
                                              "LTMAD Diff.: {round(.data$Value - .data$LTMAD,3)}")),
            alpha = 0.01, linetype = 1, size = 2)

      }
      g
    })

    dims <- c(8, 3.5) * opts$scale

    output$plot_month <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_month(), width_svg = dims[1], height_svg = dims[2],
                      options = ggiraph_opts())
    })

    # Plot --------------------
    plot_annual <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      ptiles <- ifelse(!is.null(input$ann_ptile), conseq(input$ann_ptile), NA)

      g <- create_fun(fun = "plot_annual_means", data_name = "data_flow",
                      input, input_data = data_settings(),
                      params_ignore = "discharge",
                      extra = glue::glue("percentiles_mad = {ptiles}")
      )

      code$plot <- g
      labels$plot <- "Plotting annual means"

      g <- eval_check(g)[[1]]

      # Add title
      # if(input$plot_title) {
      g <- g +
        ggplot2::ggtitle("Annual Means") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.2)))
      # }



      # Replace layers with interactive
      g <- g +
        # ggplot2::theme(legend.position = "top")+
        ggiraph::geom_bar_interactive(
          stat = "identity", alpha = 0.005,
          ggplot2::aes(tooltip = glue::glue("Year: {.data$Year}\n",
                                            "MAD: {round(.data$Mean,4)}\n",
                                            "% LTMAD: {round(.data$Mean/.data$LTMAD,3)*100}%\n",
                                            "LTMAD Diff.: {round(.data$MAD_diff, 4)}",
                                            .trim = FALSE),
                       data_id = .data$Year))+
        ggiraph::geom_hline_interactive(
          ggplot2::aes(yintercept = unique(LTMAD) - unique(LTMAD),
                       tooltip = glue::glue("LTMAD: {round(unique(.data$LTMAD),4)}")),
          alpha = 0.01, linetype = 1, size = 2)

      if (!is.null(input$ann_ptile)) {
        g <- g +
          ggiraph::geom_hline_interactive(
            ggplot2::aes(yintercept = unique(Ptile1) - unique(LTMAD),
                         tooltip = glue::glue("MAD P{input$ann_ptile[1]}: {round(.data$Ptile1[1],4)}")),
            alpha = 0.01, linetype = 1, size = 2)+
          ggiraph::geom_hline_interactive(
            ggplot2::aes(yintercept = unique(Ptile2) - unique(LTMAD),
                         tooltip = glue::glue("MAD P{input$ann_ptile[2]}: {round(.data$Ptile2[1],4)}")),
            alpha = 0.01, linetype = 1, size = 2)
      }

      g
    })

    dims_ann <- c(9, 4) * opts$scale

    output$plot_annual <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_annual(), width_svg = dims_ann[1], height_svg = dims_ann[2],
                      options = ggiraph_opts())
    })

    plot_quant <- reactive({
      check_data(data_loaded())
      req(input$duration,
          input$ret_per)

      data_flow <- data_raw()

      freqs <- create_fun(fun = "compute_annual_frequencies", data_name = "data_flow",
                          input, input_data = data_settings(), params_ignore = c("roll_days"),
                          extra = glue::glue("roll_days = {input$duration}, months = {conseq(months_all())}"))

      code$data <- freqs
      labels$data <- paste0("Compute Annual flow volume frequency analysis ",
                            "(creates all outputs as a list)")
      freqs <- eval_check(freqs)



      fit <- freqs[["Freq_Fitted_Quantiles"]] %>%
        tidyr::pivot_longer(dplyr::matches("^[0-9]+"),
                            names_to = "Measure", values_to = "Quantile") %>%
        dplyr::group_by(.data$Measure) %>%
        dplyr::mutate(
          prob1 = .data$Probability,
          prob2 = dplyr::lead(.data$Probability),
          quant1 = .data$Quantile,
          quant2 = dplyr::lead(.data$Quantile),
          Quantile = round(.data$Quantile, 4),
          `Return Period` = round(.data[["Return Period"]], 4),
          tooltip = glue::glue("Curve: {.data$Measure}<br>",
                               "Probability: {.data$Probability}<br>",
                               "Quantile: {.data$Quantile}<br>",
                               "Return Period: {.data$`Return Period`}"))


      g <- freqs[["Freq_Plot"]] +
        ggplot2::scale_y_log10(breaks = scales::pretty_breaks(n = 5),
                               labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
        #   ggplot2::ylab(ifelse(input$discharge == "Value", paste0("Yield"), paste0("Discharge cms")))+
        ggiraph::geom_point_interactive(ggplot2::aes(
          tooltip = glue::glue("Year: {.data$Year}\n",
                               "Discharge: {round(.data$Value, 4)}\n",
                               "Probability: {round(.data$prob, 4)}"),
          data_id = .data$Year), size = 2) +
        ggplot2::scale_colour_viridis_d(end = 0.8) +
        ggiraph::geom_segment_interactive(
          data = fit, ggplot2::aes(x = .data$prob1, xend = .data$prob2,
                                   y = .data$quant1, yend = .data$quant2,
                                   group = .data$Measure,
                                   tooltip = .data$tooltip),
          size = 2, alpha = 0.01)

      # Add title
      g <- g +
        ggplot2::ggtitle("Low-Flow Probabilities") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))


      g
    })

    dims_quant <- c(8, 4.5) * opts$scale
    output$plot_quant <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot_quant(),
                      width_svg = dims_quant[1],
                      height_svg = dims_quant[2],
                      options = ggiraph_opts(selection = "multiple"))
    })



    # Table -----------------------
    freq_quant <- reactive({
      check_data(data_loaded())
      req(input$duration,
          input$ret_per,
          input$freq_months)

      data_flow <- data_raw()

      t <- create_fun(fun = "compute_frequency_quantile", data_name = "data_flow",
                      input, input_data = data_settings(), params_ignore = c("roll_days", "months"),
                      extra = glue::glue("return_period = {input$ret_per},
                                         roll_days = {input$duration},
                                         months = {conseq(months_all())}"))

      code$table <- t
      labels$table <- glue::glue("Frequency Quantile")

      eval_check(t)
    })
    # Table -----------------------
    output$quant <- renderText({
      req(input$duration, input$ret_per)
      paste0(input$duration,"Q",input$ret_per, " = ", round(freq_quant(),4))
    })

    ltmads <- reactive({
      check_data(data_loaded())

      if (!is.null(input$ltmad_percent) & !all(as.numeric(input$ltmad_percent) == 100)) {
        ptiles <- input$ltmad_percent
        ptiles <- ptiles[ptiles != 100]
        ptiles <- conseq(ptiles)
      } else {
        ptiles <-  NA
      }

      data_flow <- data_raw()

      t <- create_fun(fun = "calc_longterm_mean", data_name = "data_flow",
                      input, input_data = data_settings(), params_ignore =c("percent_MAD"),
                      extra = glue::glue("percent_MAD = {ptiles}"))

      code$table <- t
      labels$table <- glue::glue("LTMADs")

      eval_check(t)
    })
    stats <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun(fun = "calc_longterm_daily_stats", data_name = "data_flow",
                      input, input_data = data_settings(),
                      params_ignore = "ignore_missing",
                      extra = "ignore_missing = TRUE, percentiles = c(10,25,75,90)")

      code$table <- t
      labels$table <- glue::glue("Stats")

      eval_check(t) %>%
        dplyr::filter(Month == "Long-term") %>%
        dplyr::select(-STATION_NUMBER, -Month, -Mean) %>%
        dplyr::mutate(stn = "A")
    })


    ltmad_table <- reactive({
      ltmads() %>%
        dplyr::mutate(stn = "A")
    })

    output$table <- renderTable({
      stats()
    })
    freq_table <- reactive({
      req(input$duration, input$ret_per)
      t <- dplyr::tibble("stn" = "A",
                         "quant" = freq_quant())
      names(t)[names(t) == "quant"] <- paste0(input$duration,"Q",input$ret_per)
      t
    })
    output$table2 <- renderTable({
      freq_table()
    })

    output$table4 <- DT::renderDT({
      dplyr::left_join(ltmad_table(), freq_table(), by = "stn") %>%
        dplyr::left_join(stats(), by = "stn") %>%
        dplyr::select(-stn) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 4),
                      dplyr::across(dplyr::everything(), as.character)) %>%
        dplyr::select(STATION_NUMBER, `Number of Years`, dplyr::everything()) %>%
        tidyr::pivot_longer(cols = 1:ncol(.)) %>%
        DT::datatable(rownames = FALSE, colnames = FALSE,
                      #filter = 'top',
                      extensions = c("Buttons"),
                      selection = "none",
                      options = list(dom ="t", buttons = list(list(extend = 'copy', title = NULL),
                                                              'csv', 'excel')))
    })
    output$table3 <- DT::renderDT({
      dplyr::left_join(ltmad_table(), freq_table(), by = "stn") %>%
        dplyr::left_join(stats(), by = "stn") %>%
        dplyr::select(-stn) %>%
        dplyr::mutate("Number of Years" = data_settings()$number_of_years) %>%
        dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 4),
                      dplyr::across(dplyr::everything(), as.character)) %>%
        dplyr::select(STATION_NUMBER, 'Number of Years', dplyr::everything()) %>%
        tidyr::pivot_longer(cols = 1:ncol(.)) %>%
        DT::datatable(rownames = FALSE, colnames = c("",""),
                      #filter = 'top',
                      extensions = c("Buttons"),
                      selection = "none",
                      options = list(ordering=FALSE, pageLength = 20,
                                     dom ="Bt", buttons = list(list(extend = 'copy', title = NULL))))
    })
















    station <- reactive({
      prep_hydat(bc_only = FALSE) %>%
        dplyr::filter(STATION_NUMBER == data_settings()$station_id)
    })

    output$station_info <- DT::renderDT({
      station()  %>%
        dplyr::mutate(LATITUDE = round(LATITUDE, 6),
                      LONGITUDE = round(LONGITUDE, 6),
                      dplyr::across(dplyr::everything(), as.character)) %>%
        tidyr::pivot_longer(cols = 1:ncol(.)) %>%
        DT::datatable(rownames = FALSE, colnames = c("", ""),
                      #filter = 'top',
                      extensions = c("Buttons"),
                      selection = "none",
                      options = list(ordering=FALSE, pageLength = 20,
                                     dom ="Bt", buttons = list(list(extend = 'copy', title = NULL))))
    })

    output$test <- renderText({
      conseq(months_all())
    })

    # # Station Map -------------------------
    # watershed_exists <- reactive({
    #
    #   if (dir.exists(paste0("data-raw/watersheds/",
    #                         substr(data_settings()$station_id,1,2),"/",data_settings()$station_id))) {
    #     t <- TRUE
    #   } else {
    #     t <- FALSE
    #   }
    #   t
    # })
    #
    # watershed <- reactive({
    #
    #   if (watershed_exists()) {
    #     suppressMessages(
    #       sf::st_read(paste0("data-raw/watersheds/",
    #                          substr(data_settings()$station_id,1,2),"/",data_settings()$station_id,"/",data_settings()$station_id,
    #                          "_DrainageBasin_BassinDeDrainage.shp")) %>%
    #         sf::st_transform(crs = 4326)
    #     )
    #   }
    # })

    output$station_map <- leaflet::renderLeaflet({

      l <- leaflet::leaflet() %>%

        # base maps
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(
          leaflet::providers$Stamen.Terrain, group = "Stamen (Terrain)") %>% #,
        #  options = leaflet::providerTileOptions(maxNativeZoom=50,maxZoom=100)) %>%
        #  leaflet::setView(zoom = 18) %>%

        # Map panes for vertical sorting
        leaflet::addMapPane("points", zIndex = 430) %>%
        leaflet::addMapPane("polygons", zIndex = 410) %>%

        # Stations
        # add_markers(data = station(), variable = "selected")  %>%
        leaflet::addCircleMarkers(
          data = station(),
          options = leaflet::pathOptions(pane = "points"),
          lng = ~LONGITUDE, lat = ~LATITUDE,
          layerId = ~STATION_NUMBER,
          radius = 6, fillOpacity = 1, stroke = TRUE,
          color = "black", opacity = 1, weight = 1,
          fillColor = "blue",
          label = ~purrr::map(glue::glue(
            "<strong>{stringr::str_to_title(STATION_NAME)}</strong><br>",
            "<strong>Station ID</strong>: {STATION_NUMBER}<br>",
            "<strong>Status</strong>: {HYD_STATUS}<br>",
            "<strong>Year range</strong>: {Year_from}-{Year_to}<br>",
            "<strong>No. Years</strong>: {RECORD_LENGTH}"), HTML)
        ) %>%

        # Controls
        leaflet::addLayersControl(
          baseGroups = c("Stamen (Terrain)", "OpenStreetMap"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )

      # #  if (watershed_exists()) {
      # #    l <- l %>% leaflet::addPolygons(data = watershed())
      # #   }
      # #   observeEvent(input$load, {
      # if (data_settings()$station_id %in% map_basins_shp$StationNum) {
      #   watershed_shp <- map_basins_shp %>%
      #     dplyr::filter(StationNum == data_settings()$station_id)
      #
      #   l <- l %>%
      #     leaflet::addPolygons(data = watershed_shp, layerId = "basin_map")
      # }

      # })

      map_ready(TRUE)
      l
    })

    map_ready <- reactiveVal(FALSE)

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))
  })

}
