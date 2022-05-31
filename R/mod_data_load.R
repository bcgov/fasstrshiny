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

ui_data_load <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Data Loading & Options"),
      box(
        width = 3,
        helpText("Select source of data below by choosing a ",
                 "station number from HYDAT or uploading a '.csv' ",
                 "file with daily mean streamflow data. View the ",
                 "data in the Daily Flow Plot and Table tabs. ",
                 "See more data options below by clicking the toggles. ",
                 "View the Data > Availability & Screening ",
                 " tabs to review data quality and availability."),
        hr(),
        # Inputs --------------------------------------------
        show_ui(ns("show_data"), "Station Selection", value = TRUE),
        div(id = ns("data_load"),

            radioGroupButtons(inputId = ns("source"),
                              label = "Source:", choices = c("HYDAT", "CSV"),
                              justified = TRUE,
                              selected = "HYDAT"),
            conditionalPanel(
              "input.source == 'HYDAT'", ns = NS(id),
              fluidRow(column(width = 6,
                              selectizeInput(ns("point_colour"), label = "Station Map Colours:",
                                             choices = maps_points),
                              bsTooltip(ns("point_colour"), placement = "left",
                                        paste0("By which variable should station markers be coloured?<br>",
                                               "RHBN = Reference Hydrometric Basin Network"))),
                       column(width = 6,br(),
                              div(id = ns("hydat_bc_tip"),
                                  prettySwitch(ns("hydat_bc"), label = "BC Stations Only",
                                               value = TRUE,status = "success", slim = FALSE, bigger = TRUE))
                       )),
              textInput(ns("station_number"), label = "Station Number:",
                        value = "08NM116",
                        placeholder = "type station number or select from map"),
              bsTooltip(ns("hydat_bc_tip"),
                        "Whether to show stations from just BC or all over Canada",
                        placement = "left"),
              bsTooltip(ns("station_number"), "HYDAT Station Number",
                        placement = "left")
            ),

            conditionalPanel(
              "input.source != 'HYDAT'", ns = NS(id),
              fileInput(ns("file"), label = "Select File:",
                        accept=c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
              uiOutput(ns("ui_file_cols"))),
            bsButton(ns("load"), "Load Data", style = "primary"),
            br(),
            hr(),
        ),


        show_ui(ns("show_stn"), "Station Information"),
        div(id = ns("stn"), uiOutput(ns("ui_stn"))),
        span(uiOutput(ns("missing_data_note")),style="color:red"),
        hr(),
        h3("Data Options"),

        show_ui(ns("show_types"), "Flow Duration and Units"),
        div(id = ns("types"),
            select_rolling(id),
            select_discharge(id)),

        show_ui(ns("show_dates"), "Years and Months"),
        div(id = ns("dates"),
            uiOutput(ns("ui_water_year")),
            uiOutput(ns("ui_months2")),
            uiOutput(ns("ui_years_range")),
            uiOutput(ns("ui_years_exclude"))
            #    uiOutput(ns("ui_months"))
        ),

        show_ui(ns("show_missing"), "Handling Missing Dates"),
        div(id = ns("missing"),
            select_complete(id),
            select_missing(id),
            select_allowed(id))
      ),

      tabBox(
        id = ns("tabs"),  width = 9,

        # HYDAT Map --------
        tabPanel(
          title = "HYDAT", value = "tabs_hydat", width = 12,
          helpText("Click on a station marker to select the station"),
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(ns("hydat_map"), width = "100%", height = "350px")
          ),
          helpText(paste0("Click on a station row to select the station, ",
                          "or filter stations and browse on the HYDAT Map")),
          DT::DTOutput(ns("hydat_table"))
          ,verbatimTextOutput(ns("test"))
        ),

        # # HYDAT Table --------
        # tabPanel(
        #   title = "HYDAT Table", width = 12,
        #
        # ),


        # CSV preview -------------
        tabPanel(
          title = "CSV Preview", value = "tabs_csv",
          h3("Loading Tips"),
          textOutput(ns("csv_status")),
          h3("CSV Preview"),
          verbatimTextOutput(ns("csv_preview")),
          h3("Loaded Data Preview"),
          verbatimTextOutput(ns("data_preview"))),


        # Plot --------
        tabPanel(
          title = "Daily Flow Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          ui_plotly_info(range = TRUE),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot"), height = opts$plot_height))
        ),

        # Table --------
        tabPanel(
          title = "Daily Flow Table",
          h4(textOutput(ns("table_title"))),
          DT::DTOutput(ns("table"))
        ),

        # R Code -----------------
        ui_rcode(id)
      )
    )
  )
}

server_data_load <- function(id) {

  moduleServer(id, function(input, output, session) {

    # Reactive Values
    data_loaded <- reactiveVal(FALSE)
    data_source <- reactiveVal("None")
    data_id <- reactiveVal("None")

    # UI Elements ---------------------------------------

    # Discharge
    observe({
      req(input$basin_area)
      s <- input$discharge
      if(input$basin_area == 0) {
        if(s == "Yield_mm") s <- "Value"
        updateAwesomeRadio(
          session, "discharge",
          choices = list("Discharge (cms)" = "Value",
                         "Volumetric Discharge (m3)" = "Volume_m3"),
          selected = s)
      } else {
        updateAwesomeRadio(
          session, "discharge",
          selected = s,
          choices = list("Discharge (cms)" = "Value",
                         "Volumetric Discharge (m3)" = "Volume_m3",
                         "Runoff Yield (mm)" = "Yield_mm"))
      }
    })

    # Toggling
    observe({
      req(!is.null(input$missing))
      if(!input$missing) updateSliderInput(session, "allowed", value = 0)
      if(input$missing) updateSliderInput(session, "allowed", value = 100)
      shinyjs::toggleState("allowed", condition = input$missing)
    })


    # Jump to tab
    observe({
      if(input$source == "HYDAT") {
        updateTabsetPanel(session, "tabs", selected = "tabs_hydat")
      } else {
        updateTabsetPanel(session, "tabs", selected = "tabs_csv")
      }
    }) %>%
      bindEvent(input$source)

    # Hide/Show based on toggle
    observe(shinyjs::toggle("data_load", condition = input$show_data))
    observe(shinyjs::toggle("stn", condition = input$show_stn))
    observe(shinyjs::toggle("dates", condition = input$show_dates))
    observe(shinyjs::toggle("types", condition = input$show_types))
    observe(shinyjs::toggle("missing", condition = input$show_missing))


    output$ui_file_cols <- renderUI({

      if(!is.null(input$file)){
        cols <- try(utils::read.csv(normalizePath(
          input$file$datapath, winslash = "/"), nrows = 1))
        validate(need(!"try-error" %in% class(cols),
                      "Cannot read this csv, is it comma-separated with headers?"))
        cols <- names(cols)
      } else cols <- c("", "", "")

      tagList(
        h4(strong("Column Names")),
        fluidRow(
          column(width = 6,
                 selectizeInput(
                   NS(id, "col_date"), width = "100%",
                   label = HTML("Dates (YYYY-MM-DD):"),
                   choices = cols, selected = cols[1])),
          column(width = 6,
                 selectizeInput(
                   NS(id, "col_value"),
                   label = HTML("Flow Values (cms):"),
                   choices = cols, selected = cols[2]))),
        fluidRow(
          column(width = 6,
                 selectizeInput(
                   NS(id, "col_symbol"),
                   label = HTML("Qualifier Symbols (optional):"),
                   choices = c(" ", cols), selected = NULL))
        ))
    })

    output$ui_stn <- renderUI({

      if(input$source == "HYDAT" && input$station_number %in% stations()$STATION_NUMBER) {
        m <- dplyr::filter(stations(), .data$STATION_NUMBER == input$station_number)
        stn_name <- m$STATION_NAME
        basin <- as.numeric(m$DRAINAGE_AREA_GROSS)
      } else if(!is.null(input$file)) {
        stn_name <- basename(input$file$name)
        basin <- 0
      } else {
        stn_name <- ""
        basin <- 0
      }

      fluidRow(id = NS(id, "station_info"),
               column(
                 width = 6,
                 textInput(NS(id, "station_name"),
                           label = "Station Name:", value = stn_name,
                           placeholder = "ex. Mission Creek")),
               column(
                 width = 6,
                 numericInput(NS(id, "basin_area"),
                              label = HTML("Basin Area (km<sup>2</sup>):"),
                              value = basin,
                              min = 0, step = 0.1)),
               bsTooltip(NS(id, "station_info"),
                         paste0("Station Name for context<p><p>",
                                tips$basin_area),
                         placement = "left"))
    })

    output$ui_water_year <- renderUI({
      tagList(
        selectInput(
          NS(id, "water_year"),
          label = "Water Year Start Month:",
          choices = stats::setNames(1:12, month.abb),
          selected = 1,# size = "sm",
          width = "100%"),
        bsTooltip(NS(id, "water_year"),
                  title = tips$water_year, placement = "left"))
    })

    output$ui_years_range <- renderUI({
      tagList(
        sliderInput(NS(id, "years_range"),
                    label = "Start and End Years:",
                    min = min(data_raw()$WaterYear),
                    max = max(data_raw()$WaterYear),
                    value = c(min(data_raw()$WaterYear),
                              max(data_raw()$WaterYear)),
                    dragRange = TRUE, sep = "", step = 1),
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
                       label = "Years to Exclude:",
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
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(input$water_year)],
             m[m < as.numeric(input$water_year)])

      t <- tagList(
        selectizeInput(NS(id, "months"),
                       label = "Months to Include:",
                       choices = m,
                       selected = 1:12,
                       multiple = TRUE),
        bsTooltip(NS(id, "months"), tips$months, placement = "left"))
      return(t)
    })


    output$ui_months2 <- renderUI({
      req(input$water_year)

      # Arrange months by water year
      m <- stats::setNames(1:12, month.abb)
      m <- c(m[m >= as.numeric(input$water_year)],
             m[m < as.numeric(input$water_year)])

      tagList(
        sliderTextInput(
          inputId = NS(id, "months2"),
          label = "Months to Include:",
          choices = month.abb[m],
          selected = (month.abb[m])[c(1,12)],
          grid = TRUE,
          hide_min_max = TRUE
        ),
        bsTooltip(NS(id, "months2"), tips$months, placement = "left")
      )
    })

    months_all <- reactive({
      req(input$months2)

      mn <- c(stats::setNames(1:12, month.abb),stats::setNames(1:12, month.abb))
      mn_list <- mn[which(names(mn) == input$months2[1], mn)[1]:length(mn)]
      mn_list <- mn_list[1:which(names(mn_list) == input$months2[2], mn_list)[1]]
      mn_list

    })


    complete_years <- reactive({
      req(input$water_year)

      d <- data_raw() %>%
        fasstr::fill_missing_dates(water_year_start = as.numeric(input$water_year)) %>%
        fasstr::add_date_variables(water_year_start = as.numeric(input$water_year)) %>%
        dplyr::group_by(WaterYear) %>%
        dplyr::summarise(n_na = sum(is.na(Value)),
                         n = dplyr::n(),
                         empty = ifelse(n_na == n, TRUE, FALSE), .groups = "keep")

      complete <- d %>%
        dplyr::filter(n_na == 0) %>%
        dplyr::pull(WaterYear)
      incomplete <- d %>%
        dplyr::filter(n_na > 0) %>%
        dplyr::pull(WaterYear)
      empty <- d %>%
        dplyr::filter(empty) %>%
        dplyr::pull(WaterYear)
      #  empty <- ifelse(length(empty)==0, NA, empty)
      list(complete_years = complete,
           incomplete_years = incomplete,
           comp_start = min(complete, na.rm = TRUE),
           comp_end = max(complete, na.rm = TRUE),
           empty = empty)
    })

    observe({
      req(!is.null(input$complete))
      if(input$complete) updateSliderInput(session, "years_range", value = c(complete_years()$comp_start,
                                                                             complete_years()$comp_end))
      if(input$complete) updateSelectizeInput(session, "years_exclude", selected = complete_years()$incomplete)
    })





    # Add plot options as Gear in corner
    output$ui_plot_options <- renderUI({
      req(data_loaded())
      select_plot_options(
        select_plot_title(id),
        select_plot_log(id, value = default("plot_flow_data", "log_discharge")),
        select_daterange(id, data_raw()))
    })


    # Bookmarking -----
    # Preserve dynamic UI inputs during bookmarking
    keep <- c("col_date", "col_value", "col_symbol",
              "plot_title", "plot_log", "station_name", "basin_area",
              "water_year", "years_range", "years_exclude", "months")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))

    # Update station from Map button
    observe({
      updateTextInput(session, "station_number",
                      value = input$hydat_map_marker_click$id)
    }) %>%
      bindEvent(input$hydat_map_marker_click)

    # Update station from Table button
    observe({
      updateTextInput(
        session, "station_number",
        value = stations()$STATION_NUMBER[input$hydat_table_rows_selected])
    }) %>%
      bindEvent(input$hydat_table_rows_selected)

    # CSV Details ------------------
    output$csv_status <- renderText({
      validate(
        need(input$file, "Select a file to upload") %then%

          need(length(unique(c(input$col_date, input$col_value,
                               input$col_symbol))) == 3,
               "Date, Value and Symbol columns must be distinct") %then%

          need(!is.null(input$basin_area) && input$basin_area > 0,
               paste0("If you have it, specify basin area under ",
                      "Station Information; Otherwise, ",
                      "click the Load Data button!")) %then%

          need(data_id() == input$file$name,
               "Click the Load Data button!"),

        errorClass = "helper")

    })

    output$csv_preview <- renderText({
      req(input$file)
      readLines(input$file$datapath, n = 6) %>%
        paste0(collapse = "\n") %>%
        paste0("\n...")
    })
    output$data_preview <- renderText({
      req(data_source() == "CSV" & data_id() == input$file$name)
      if(data_source() == "CSV" && !is.null(input$file$name) &&
         data_id() == input$file$name) {
        need(!any(duplicated(data_raw()$Date)),
             paste0("There are duplicate dates in the data... ",
                    "is this from a single station?")) %>%
          validate(errorClass = "red")
      }
      req(data_loaded())
      dplyr::slice(data_raw(), 1:6) %>%
        utils::capture.output() %>%
        paste0(collapse = "\n") %>%
        paste0("\n...")
    })


    # HYDAT Stations -------------------
    stations <- reactive({
      req(!is.null(input$hydat_bc))
      prep_hydat(bc_only = input$hydat_bc) %>%
        dplyr::mutate(
          selected = .data$STATION_NUMBER == isolate(input$station_number))
    }) %>%
      bindCache(input$hydat_bc) %>%
      bindEvent(input$hydat_bc)

    stations_sub <- reactive({
      if(is.null(sub <- input$hydat_table_rows_all)) sub <- 1:nrow(stations())

      stations() %>%
        dplyr::slice(.env$sub) %>%
        dplyr::mutate(selected = .data$STATION_NUMBER == input$station_number) %>%
        dplyr::arrange(.data$selected)
    })

    # HYDAT Map -------------------------
    output$test <- renderPrint({
      #  req(!is.null(input$months2))
      watershed_exists()
    })
    watershed_exists <- reactive({
      req(input$station_number)

      if (dir.exists(paste0("data-raw/watersheds/",
                            substr(input$station_number,1,2),"/",input$station_number))) {
        t <- TRUE
      } else {
        t <- FALSE
      }
      t
    })

    watershed <- reactive({
      req(input$station_number)

      if (watershed_exists()) {
        suppressMessages(
          sf::st_read(paste0("data-raw/watersheds/",
                             substr(input$station_number,1,2),"/",input$station_number,"/",input$station_number,
                             "_DrainageBasin_BassinDeDrainage.shp")) %>%
            sf::st_transform(crs = 4326)
        )
      }
    })

    pal <- reactive({
      req(input$point_colour)

      if(input$point_colour %in% c("DRAINAGE_AREA_GROSS", "RECORD_LENGTH")) {
        leaflet::colorNumeric("viridis", domain = stations()[[input$point_colour]])
      } else {
        leaflet::colorFactor("viridis", domain = stations()[[input$point_colour]])
      }
    })

    add_markers <- function(map, data, variable) {
      selected <- variable == "selected"
      if(selected) {
        data <- data[data$selected, ]
        fill <- "red"
      } else {
        data <- data[!data$selected, ]
        fill <- pal()(data[[variable]])
      }

      if(nrow(data) > 0) {
        map <- leaflet::addCircleMarkers(
          map, data = data,
          group = "points",
          options = leaflet::pathOptions(pane = "points"),
          lng = ~LONGITUDE, lat = ~LATITUDE,
          layerId = ~STATION_NUMBER,
          radius = 6, fillOpacity = 1, stroke = TRUE,
          fillColor = fill,
          color = "black", opacity = 1, weight = 1,
          label = ~purrr::map(glue::glue(
            "<strong>{stringr::str_to_title(STATION_NAME)}</strong><br>",
            "<strong>Station ID</strong>: {STATION_NUMBER}<br>",
            "<strong>Status</strong>: {HYD_STATUS}<br>",
            "<strong>Year range</strong>: {Year_from}-{Year_to}<br>",
            "<strong>No. Years</strong>: {RECORD_LENGTH}"), HTML))
      }
      map
    }

    output$hydat_map <- leaflet::renderLeaflet({
      req(input$point_colour)

      l <- leaflet::leaflet() %>%

        # base maps
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(
          leaflet::providers$Stamen.Terrain, group = "Stamen (Terrain)") %>%

        # Map panes for vertical sorting
        leaflet::addMapPane("points", zIndex = 430) %>%
        leaflet::addMapPane("polygons", zIndex = 410) %>%

        # Stations
        add_markers(data = stations(), variable = input$point_colour) %>%
        add_markers(data = stations(), variable = "selected") %>%
        leaflet::addLegend(
          "bottomright", pal = pal(),
          values = stations()[[input$point_colour]],
          title = names(maps_points[maps_points == input$point_colour])) %>%

        # Controls
        leaflet::addLayersControl(
          baseGroups = c("Stamen (Terrain)", "OpenStreetMap"),
          overlayGroups = bc_maps_labs$group,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )

      if (watershed_exists()) {
        l <- l %>% leaflet::addPolygons(data = watershed())
      }

      # Hide all polygons
      for(i in bc_maps_labs$group) l <- leaflet::hideGroup(l, i)

      for(i in seq_along(bc_maps_layers)) {
        l <- l %>%
          leaflet::addPolygons(
            options = leaflet::pathOptions(pane = "polygons"),
            data = bc_maps_layers[[i]], group = bc_maps_labs$group[[i]],
            stroke = 0.5, opacity = 1, weight = 1,
            fillOpacity = 0.15, fillColor = "black", color = "black",
            label = bc_maps_labs$label[[i]])
      }

      map_ready(TRUE)
      l
    })

    map_ready <- reactiveVal(FALSE)

    observe({
      req(input$point_colour)
      leaflet::leafletProxy("hydat_map") %>%
        leaflet::clearGroup("points") %>%
        add_markers(data = stations_sub(), variable = input$point_colour) %>%
        add_markers(data = stations_sub(), variable = "selected")
    }) %>%
      bindEvent(stations_sub())



    # HYDAT Table ------------------------
    output$hydat_table <- DT::renderDT({
      stations() %>%
        DT::datatable(selection = "single", rownames = FALSE, filter = 'top',
                      extensions = c("Scroller", "Buttons"),
                      options = list(scrollX = TRUE,
                                     scrollY = 450, deferRender = TRUE,
                                     scroller = TRUE,
                                     dom = 'Brtip',
                                     buttons = list(list(extend = 'copy', title = NULL),
                                                    'csv', 'excel')),
        )
    })


    # Add dates and discharge
    add_dates_discharge <- function(d) {
      d <- glue::glue(
        "{d} %>%\n",
        "add_date_variables(water_year_start = {as.numeric(input$water_year)}) %>%",
        "  add_daily_volume()")
      if(input$basin_area > 0) {
        d <- glue::glue("{d} %>% add_daily_yield(basin_area = {input$basin_area})")
      }
      d
    }


    # Raw data - HYDAT ---------------
    data_raw_hydat <- reactive({
      req(input$water_year, input$load > 0, input$station_number)
      glue::glue(
        "data_flow <- fill_missing_dates(",
        "  station_number = '{input$station_number}')") %>%
        add_dates_discharge()
    })

    # Raw data - File ---------
    data_raw_file <- reactive({
      req(input$file, input$col_date, input$col_value)

      validate(need(length(unique(c(
        input$col_date, input$col_value, input$col_symbol))) == 3,
        "Date, Value and Symbol columns must be distinct"), errorClass = "red")

      # Get proper paths for diff OS (Need "/" otherwise parse() has issues)
      f <- normalizePath(input$file$datapath, winslash = "/")

      glue::glue(
        "data_flow <- read.csv('{f}') %>% ",
        "  dplyr::select(Date = {input$col_date}, Value = {input$col_value}",
        dplyr::if_else(!input$col_symbol %in% c("", " "), ", Symbol = {input$col_symbol}", ""),
        ") %>% fill_missing_dates()") %>%
        add_dates_discharge()
    })


    # Raw data ------------------
    data_raw <- reactive({

      if (input$source == "HYDAT") {
        d <- code$data_raw <- data_raw_hydat()
        data_source("HYDAT")
        data_id(input$station_number)

      } else if (input$source == "CSV") {
        d <- data_raw_file()
        data_source("CSV")
        data_id(basename(input$file$name))

        code$data_raw <- stringr::str_replace( # Make pretty for R Code Tab
          d,
          "^data_flow <- read.csv\\([^\\)]+\\)",
          glue::glue("data_flow <- read.csv('{input$file$name}')"))
      }

      labels$data_raw <- "Load data, prep dates, calculate volume/yield"

      d <- eval_check(d)

      if(data_source() == "CSV" && !is.null(input$file$name) &&
         data_id() == input$file$name) {
        need(!any(duplicated(d$Date)),
             paste0("There are duplicate dates in the data... ",
                    "is this from a single station?")) %>%
          validate(errorClass = "red")
      }

      data_loaded(TRUE)
      d
    }) %>%
      bindEvent(input$load, input$water_year, input$basin_area,
                ignoreInit = TRUE)

    # Missing data note -----------------

    data_raw_missing <- reactive({
      req(input$years_range,
          # input$months,
          input$discharge)

      m <- data_raw() %>%
        dplyr::select(-"Month") %>%
        dplyr::filter(.data$WaterYear >= input$years_range[1],
                      .data$WaterYear <= input$years_range[2],
                      .data$MonthName %in% month.abb[months_all()])

      if(!is.null(input$years_exclude)) m <- dplyr::filter(m, !.data$WaterYear %in% input$years_exclude)

      dplyr::pull(m, input$discharge)
    })


    output$missing_data_note <- renderUI({

      req(any(is.na(data_raw_missing())))

      #   req(any(is.na(data_raw()[[input$discharge]])))

      tagList(h5("Missing Data Note:"),
              "There is missing data in the range of dates selected. ",
              "Some plots and tables may not show results.",
              "See 'Handling Missing Dates' below for options if desired.")
    })


    number_of_years <- reactive({
      data_raw() %>%
        dplyr::filter(.data$WaterYear >= input$years_range[1],
                      .data$WaterYear <= input$years_range[2],
                      !.data$WaterYear %in% input$years_exclude,
                      !.data$WaterYear %in% complete_years()$empty) %>%
        dplyr::pull(.data$WaterYear) %>%
        unique() %>%
        length()
    })

    # Plot ----------------
    output$plot <- plotly::renderPlotly({
      check_data(data_loaded())
      check_yield(data_settings())
      req(input$daterange, input$years_range, input$water_year, input$discharge)

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_flow_data", data_name = "data_flow", input)

      code$data_plot <- g
      labels$data_plot <- "Plot general flows"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(title(data_settings(), "Daily Flow")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      g %>%
        plotly::ggplotly(dynamicTicks = TRUE) %>%
        plotly::rangeslider() %>%
        plotly::config(modeBarButtonsToRemove =
                         c("pan", "autoscale", "zoomIn2d", "zoomOut2d",
                           "lasso2d", "select2d",
                           "hoverCompareCartesian", "hoverClosestCartesian"))
    }) %>%
      bindCache(data_raw(), data_settings(),
                input$plot_title, input$plot_log, input$daterange)

    # Table ----------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$years_range)
      data_raw() %>%
        dplyr::select(-"Month") %>%
        dplyr::filter(.data$WaterYear >= input$years_range[1],
                      .data$WaterYear <= input$years_range[2],
                      !.data$WaterYear %in% input$years_exclude,
                      .data$MonthName %in% month.abb[as.numeric(months_all())]) %>%
        prep_DT()
    })

    output$table_title <- renderText({
      title(data_settings(), "Daily Flow data")
    })

    # R Code ----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels,
                                          order = c("data_raw", "data_plot")))


    # Sidebar: Data Info ------------------------
    output$info <- gt::render_gt({

      d <- dplyr::tibble(name = "", value = "", .rows = 0)
      t <- "Current: No Data"
      s <- NULL
      foot <- FALSE

      if(data_loaded() & !is.null(input$years_range)) {

        if(is.null(input$years_exclude)) {
          ye <- ""
        } else ye <- conseq(input$years_exclude, wrap = FALSE)

        t <- glue::glue("Station: {data_id()}")
        if(input$basin_area > 0) {
          t <- glue::glue("{t} <small>({input$basin_area} km<sup>2</sup>)</small>")
        }
        s <- input$station_name

        m <- conseq_wy(as.numeric(months_all()),
                       wy = as.numeric(input$water_year))

        n <- data_raw() %>%
          dplyr::filter(.data$WaterYear >= input$years_range[1],
                        .data$WaterYear <= input$years_range[2],
                        !.data$WaterYear %in% input$years_exclude,
                        !.data$WaterYear %in% complete_years()$empty) %>%
          dplyr::pull(.data$WaterYear) %>%
          unique() %>%
          length()

        if (length(complete_years()$empty) == 0) {
          n_empty <- ""
        } else {
          if (any(complete_years()$empty %in% input$years_range[1]:input$years_range[2])) {
            n_empty <- paste0(' (',length(complete_years()$empty), ' no data)')
          } else {
            n_empty <- ""
          }
        }

        wy <- as.numeric(input$water_year)
        wy <- c(wy, wy-1)
        if(wy[2] == 0) wy[2] <- 12
        wy <- conseq(wy, type = "month") %>% stringr::str_replace(", ", "-")

        yr <- glue::glue_collapse(input$years_range, sep = "-") %>%
          glue::glue(" ({input$years_range[2] - input$years_range[1] + 1} yrs)")

        units <- switch(input$discharge,
                        "Value" = "cms",
                        "Volume_m3" = "m<sup>3</sup>",
                        "Yield_mm" = "mm")

        #  if(any(is.na(data_raw()[[input$discharge]]))) foot <- TRUE
        if(any(is.na(data_raw_missing())) & (!input$missing | input$allowed == 100)) foot <- TRUE

        d <- list(
          `Water Year` = wy,
          `Year Range` = yr,
          `Years Excl.` = ye,
          `Total Years` = glue::glue(
            "{n} / {input$years_range[2] - input$years_range[1] + 1}{n_empty}"),
          `Months` = m,
          `Rolling Avg. Days` = as.character(input$roll_days),
          `Discharge Units` = units,
          `Ignore Missing` = dplyr::if_else(input$missing, "Yes", "No"),
          `Allowed Missing` = glue::glue("{input$allowed}%")) %>%
          tibble::enframe() %>%
          tidyr::unnest(.data$value)
      }

      g <- gt::gt(d) %>%
        gt::cols_align("left") %>%
        gt::cols_width(name ~ px(100)) %>%
        gt::tab_header(title = gt::html(t), subtitle = gt::html(s)) %>%
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
        gt::fmt_markdown(columns = "value") %>%
        gt::tab_style(
          style = gt::cell_borders(sides = c("top", "bottom"), weight = 0),
          locations = gt::cells_body(columns = gt::everything(),
                                     rows = gt::everything())) %>%
        gt::tab_style(
          style = gt::cell_text(size = "small"),
          locations = gt::cells_title(groups = "subtitle"))

      if(nrow(d) > 0) {
        g <- g %>%
          gt::tab_style(
            style = gt::cell_borders(sides = "bottom", weight = 1, color = "#B8C7CE"),
            locations = gt::cells_body(columns = gt::everything(), rows = 5))
      }

      if(any(is.na(data_raw_missing())) & (!input$missing | input$allowed == 100)) foot <- TRUE

      msg_missing <-  paste0(
        "Note: there is missing flow data in the selected date range and may result in NAs or blank plots. ",
        "Go to Data >> Loading >> Handling Missing Dates to manage missing dates.")

      if(foot) g <- gt::tab_source_note(g, msg_missing)

      g
    })

    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(id, output)


    # Outputs ------------------------
    data_settings <- reactive({
      list("discharge" = input$discharge,
           "water_year" = input$water_year,
           "years_range" = input$years_range,
           "years_range_orig" = c(min(data_raw()$WaterYear),
                                  max(data_raw()$WaterYear)),
           "years_exclude" = input$years_exclude,
           "months" = months_all(),
           "roll_days" = input$roll_days,
           "roll_align" = input$roll_align,
           "complete" = input$complete,
           "missing" = input$missing,
           "allowed" = input$allowed,
           "basin_area" = input$basin_area,
           "station_name" = input$station_name,
           "station_id" = data_id(),
           "source" = data_source(),
           "missing_note" = ifelse(any(is.na(data_raw_missing())),TRUE,FALSE),
           "number_of_years" = number_of_years())
    })

    data_code <- reactive(code$data_raw)

    list(
      data_settings = data_settings,
      data_raw = data_raw,
      data_loaded = data_loaded,
      data_code = data_code)
  })
}

