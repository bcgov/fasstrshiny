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

# Data Loading ------------------


ui_data_load <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Loading Data"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        radioGroupButtons(inputId = ns("source"),
                          label = "Source", choices = c("HYDAT", "CSV"),
                          justified = TRUE,
                          selected = "HYDAT"),

        conditionalPanel(
          "input.source == 'HYDAT'", ns = NS(id),
          textInput(ns("station_num"), label = "Station Number",
                    value = "08HB048",
                    placeholder = "type station number or select from map")),

        conditionalPanel(
          "input.source != 'HYDAT'", ns = NS(id),
          fileInput(ns("file"), label = "Select File",
                    accept=c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))),
        bsButton(ns("load"), "Load Data", style = "primary"),
        hr(),

        # show_ui("show_stn", "Station Information"),
        # fluidRow(id = ns("stn"),
        #          column(
        #            width = 6,
        #            textInput(ns("station_name"),
        #                      label = "Name",
        #                      placeholder = "ex. Mission Creek")),
        #          column(
        #            width = 6,
        #            numericInput(ns("basin_area"),
        #                         label = html("Basin area (km<sup>2</sup>)"), value = 0,
        #                         min = 0, step = 0.1))),
        show_ui(ns("show_dates"), "Dates"),
        div(id = ns("dates"),
            uiOutput(ns("ui_water_year")),
            uiOutput(ns("ui_years_range")),
            uiOutput(ns("ui_years_exclude")),
            uiOutput(ns("ui_months"))),

        show_ui(ns("show_types"), "Data types"),
        div(id = ns("types"),
            select_rolling(id),
            select_discharge(id),
            select_complete(id),
            select_missing(id),
            select_allowed(id))
      ),

      tabBox(
        width = 9,

        ### HYDAT Map --------
        tabPanel(
          title = "HYDAT Map", width = 12,
          leaflet::leafletOutput(ns("hydat_map"), width = "100%", height = "500px")
        ),

        ### HYDAT Table --------
        tabPanel(
          title = "HYDAT Table", width = 12,
          DT::DTOutput(ns("hydat_table"))
        ),

        ### Plot --------
        tabPanel(
          title = "Plot", value = "tabs_plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot"), height = opts$plot_height))
        ),

        ### Table --------
        tabPanel(
          title = "Table",
          DT::DTOutput(ns("table"))
        ),

        ### R Code -----------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput(ns("code"))
        )
      )
    )
  )
}

server_data_load <- function(id, stations, bc_hydrozones) {

  moduleServer(id, function(input, output, session) {

    # Reactive Values
    code <- reactiveValues()
    meta <- reactiveValues(station_id = "",
                           station_name = "",
                           basin_area = NA_real_)

    # Data loaded
    data_loaded <- reactiveVal(FALSE)
    observe(data_loaded(TRUE)) %>% bindEvent(data_raw())

    # UI elements ---------------------------------------
    output$ui_water_year <- renderUI({
      tagList(
        radioGroupButtons(
          NS(id, "water_year"),
          label = "Water year start",
          choices = setNames(1:12, month.abb),
          selected = 1, size = "sm", width = "100%"),
        bsTooltip(NS("water_year", id),
                  title = tips$water_year, placement = "left"))
    })

    output$ui_years_range <- renderUI({
      tagList(
        sliderInput(NS(id, "years_range"),
                    label = "Start and end years",
                    min = min(data_raw()$WaterYear),
                    max = max(data_raw()$WaterYear),
                    value = c(min(data_raw()$WaterYear),
                              max(data_raw()$WaterYear)),
                    dragRange = TRUE, sep = ""),
        bsTooltip(NS(id, "years_range"), title = tips$years_range,
                  placement = "left"))
    })

    output$ui_years_exclude <- renderUI({
      req(input$years_range)

      # If updating, use old values where within range
      isolate({
        if(!is.null(input$years_exclude)) {
          s <- as.numeric(input$years_exclude)
          s <- s[s >= input$years_range[1] & s <= input$years_range[2]]
        } else s <- NULL
      })

      tagList(
        selectizeInput(NS(id, "years_exclude"),
                       label = "Years to exclude",
                       choices = seq(from = input$years_range[1],
                                     to = input$years_range[2], by = 1),
                       selected = s,
                       multiple = TRUE),
        bsTooltip(NS(id, "years_exclude"), title = tips$years_exclude,
                  placement = "left"))
    })

    output$ui_months <- renderUI({
      req(input$water_year)

      # Arrange months by water year
      m <- setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(input$water_year)],
             m[m < as.numeric(input$water_year)])

      tagList(
        selectizeInput(NS(id, "months"),
                       label = "Months to Include",
                       choices = m,
                       selected = 1:12,
                       multiple = TRUE),
        bsTooltip(NS(id, "months"), tips$months, placement = "left"))
    })

    # Update station from Map button
    observe({
      updateTextInput(session, "station_num",
                      value = input$hydat_map_marker_click$id)
    }) %>%
      bindEvent(input$hydat_map_marker_click)

    # Update station from Table button
    observe({
      updateTextInput(
        session, "station_num",
        value = stations$station_number[input$hydat_table_rows_selected])
    }) %>%
      bindEvent(input$hydat_table_rows_selected)


    # Add plot options as Gear in corner
    output$ui_plot_options <- renderUI({
      select_plot_options(
        select_plot_log(id, value = formals(plot_flow_data)$log_discharge), # Default
        select_daterange(id, data_raw()))
    })


    # Hide/Show based on toggle
    observe(shinyjs::toggle("stn", condition = input$show_stn))
    observe(shinyjs::toggle("dates", condition = input$show_dates))
    observe(shinyjs::toggle("types", condition = input$show_types))


    ## HYDAT Map -------------------------
    output$hydat_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(
          leaflet::providers$Stamen.TonerLite,
          options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
        leaflet::addPolygons(
          data = bc_hydrozones,
          stroke = 0.5, opacity = 1, weight = 1,
          fillOpacity = 0.15, fillColor = "black", color = "black",
          label = ~stringr::str_to_title(HYDROLOGICZONE_NAME)) %>%
        leaflet::addCircleMarkers(
          data = stations, lng = ~longitude, lat = ~latitude,
          layerId = ~ station_number,
          radius = 3, fillOpacity = 1, stroke = FALSE, color = "#31688E",
          label = ~ station_number,
          popup = ~glue::glue("<strong>Station Name:</strong> ",
                              "{stringr::str_to_title(station_name)}<br>",
                              "<strong>Station Number:</strong> {station_number}"))
    })

    ## HYDAT Table ------------------------
    output$hydat_table <- DT::renderDT({
      stations %>%
        dplyr::select("station_number", "station_name", "province",
                      "hyd_status", "real_time", "regulated", "parameters") %>%
        DT::datatable(selection = "single", rownames = FALSE, filter = 'top',
                      extensions = c("Scroller"),
                      options = list(scrollX = TRUE,
                                     scrollY = 450, deferRender = TRUE,
                                     scroller = TRUE,
                                     dom = 'Brtip'))
    })

    ## Raw data ------------------
    data_raw <- reactive({
      req(input$station_num, input$water_year, input$load > 0)

      if (input$source == "HYDAT") {
        m <- dplyr::filter(stations, .data$station_number == input$station_num)
        meta$station_id <- input$station_num
        meta$station_name <- m$station_name
        meta$basin_area <- as.numeric(m$drainage_area_gross)

        d <- glue::glue(
          "data_flow <- fill_missing_dates(",
          "        station_number = '{input$station_num}') %>%",
          "  add_date_variables(water_year_start = {as.numeric(input$water_year)}) %>%",
          "  add_daily_volume() %>%",
          "  add_daily_yield()")

      } else {
        inFile <- input$file
        if (is.null(inFile)) return(NULL)

        meta$station_id <- basename(input$file)
        meta$station_name <- input$station_name
        meta$basin_area <- as.numeric(input$basin_area)

        d <- glue::glue(
          "data_flow <- read.csv({inFile$datapath}) %>%",
          "  fill_missing_dates() %>%",
          "  add_date_variables(water_year_start = {as.numeric(input$water_year)})")

      }

      # Save unevaluated code for code tab
      code$data_raw <- d

      data_loaded(TRUE)

      eval(parse(text = d)) # Evaluate and create data_flow
    }) %>%
      bindEvent(input$load, input$water_year,
                ignoreInit = TRUE)

    ## Plot ----------------
    output$plot <- plotly::renderPlotly({
      check_data(data_loaded())
      req(input$daterange,
          input$years_range,
          input$water_year)

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_flow_data", data_name = "data_flow", input)

      code$data_plot <- g

      parse(text = g) %>%
        eval() %>%
        .[[1]] %>%
        plotly::ggplotly() %>%
        plotly::config(modeBarButtonsToRemove =
                 c("pan", "autoscale", "zoomIn2d", "zoomOut2d",
                   "hoverCompareCartesian", "hoverClosestCartesian"))
    })

    ## Table ----------------
    output$table <- DT::renderDT({
      req(input$years_range)

      check_data(input)
      data_raw() %>%
        dplyr::rename("StationNumber" = "STATION_NUMBER") %>%
        dplyr::select(-"Month") %>%
        dplyr::filter(WaterYear >= input$years_range[1],
                      WaterYear <= input$years_range[2],
                      !WaterYear %in% input$years_exclude,
                      MonthName %in% month.abb[as.numeric(input$months)]) %>%
        dplyr::mutate(Value = round(Value, 4)) %>%
        DT::datatable(rownames = FALSE,
                      filter = 'top',
                      extensions = c("Scroller"),
                      options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                                     deferRender = TRUE, dom = 'Bfrtip'))
    })

    ## R Code ----------------
    output$code <- renderText({
      code_format(code)
    })


    ## Sidebar: Data Info ------------------------
    output$info <- gt::render_gt({

      d <- dplyr::tibble(name = "", value = "", .rows = 0)
      t <- "Current Data: None"
      s <- NULL

      if(!is.null(input$water_year) & !is.null(input$years_range) &
         !is.null(input$months)) {

        if(is.null(input$years_exclude)) {
          ye <- ""
        } else ye <- glue::glue_collapse(input$years_exclude, sep = ", ")

        t <- glue::glue("Current Data: {meta$station_id}")
        s <- meta$station_name

        m <- input$months
        if(all(1:12 %in% m)) m <- "all" else m <- glue::glue_collapse(m, sep = ", ")

        n <- data_raw() %>%
          dplyr::filter(WaterYear >= input$years_range[1],
                        WaterYear <= input$years_range[2],
                        !WaterYear %in% input$years_exclude) %>%
          dplyr::pull(WaterYear) %>%
          unique() %>%
          length()


        d <- list(`Water Year` = month.abb[as.numeric(input$water_year)],
                  `Year Range` = glue::glue_collapse(input$years_range, sep = "-"),
                  `Years Excl.` = ye,
                  `Total Years` = as.character(n),
                  `Months` = m) %>%
          tibble::enframe() %>%
          tidyr::unnest(value)
      }

      gt::gt(d) %>%
        gt::cols_align("left") %>%
        gt::cols_width(name ~ px(65)) %>%
        gt::tab_header(title = t, subtitle = s) %>%
        gt::tab_options(column_labels.hidden = TRUE,
                        heading.subtitle.font.size = 14,
                        heading.align = "left",
                        #table.border.top.width = 0,
                        table.border.bottom.width = 0,
                        heading.border.bottom.width = 0,
                        table.font.size = 12,
                        data_row.padding = 1,
                        table.background.color = "#FFFFFF00",  # Transparent
                        table.font.color = "#b8c7ce",
                        table.align = "center",
                        table.width = "90%") %>%
        gt::tab_style(
          style = gt::cell_borders(
            sides = c("top", "bottom"),
            weight = 0),
          locations = gt::cells_body(
            columns = gt::everything(),
            rows = gt::everything()
          ))
    })

    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(id, output)


    # Outputs ------------------------
    data_settings <- reactiveValues()
    observe({
      data_settings[["discharge"]] <- input$discharge
      data_settings[["water_year"]] <- input$water_year
      data_settings[["years_range"]] <- input$years_range
      data_settings[["years_exclude"]] <- input$years_exclude
      data_settings[["months"]] <- input$months
      data_settings[["roll_days"]] <- input$roll_days
      data_settings[["roll_align"]] <- input$roll_align
      data_settings[["complete"]] <- input$complete
      data_settings[["missing"]] <- input$missing
      data_settings[["allowed"]] <- input$allowed
    })

    list(
      data_settings = data_settings,
      data_raw = data_raw,
      data_loaded = data_loaded)
  })
}

