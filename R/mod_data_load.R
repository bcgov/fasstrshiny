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
          div(id = ns("hydat_bc_tip"),
              prettySwitch(ns("hydat_bc"), label = "BC stations only",
                           value = TRUE,status = "success", slim = TRUE)),
          textInput(ns("station_number"), label = "Station Number",
                    value = "08HB048",
                    placeholder = "type station number or select from map"),
          bsTooltip(ns("hydat_bc_tip"),
                    "Whether to show stations from just BC or all over Canada",
                    placement = "left"),
          bsTooltip(ns("station_number"), "HYDAT Station Number",
                    placement = "left")
          ),

        conditionalPanel(
          "input.source != 'HYDAT'", ns = NS(id),
          fileInput(ns("file"), label = "Select File",
                    accept=c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
          uiOutput(ns("ui_file_cols"))),
        bsButton(ns("load"), "Load Data", style = "primary"),
        hr(),

        show_ui(ns("show_stn"), "Station Information"),
        div(id = ns("stn"), uiOutput(ns("ui_stn"))),

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
        id = ns("tabs"),  width = 9,

        # HYDAT Map --------
        tabPanel(
          title = "HYDAT Map", value = "tabs_hydat", width = 12,
          helpText("Click on a station marker to select the station"),
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(ns("hydat_map"), width = "100%", height = "500px")
          )
        ),

        # HYDAT Table --------
        tabPanel(
          title = "HYDAT Table", width = 12,
          helpText(paste0("Click on a station row to select the station, ",
                          "or filter stations and browse on the HYDAT Map")),
          DT::DTOutput(ns("hydat_table"))
        ),


        # CSV preview -------------
        tabPanel(
          title = "CSV Preview", value = "tabs_csv",
          h3("Loading Tips"),
          textOutput(ns("csv_status")),
          textOutput(ns("csv_checks")),
          h3("Preview"),
          verbatimTextOutput(ns("csv_preview"))),


        # Plot --------
        tabPanel(
          title = "Plot",
          uiOutput(ns("ui_plot_options"), align = "right"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot"), height = opts$plot_height))
        ),

        # Table --------
        tabPanel(
          title = "Table",
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
    data_type <- reactiveVal("None")
    data_id <- reactiveVal("None")

    # UI Elements ---------------------------------------

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
    observe(shinyjs::toggle("stn", condition = input$show_stn))
    observe(shinyjs::toggle("dates", condition = input$show_dates))
    observe(shinyjs::toggle("types", condition = input$show_types))


    output$ui_file_cols <- renderUI({

      if(!is.null(input$file)){
        cols <- try(read.csv(input$file$datapath, nrows = 1))
        validate(need(!"try-error" %in% class(cols),
                      "Cannot read this csv, is it comma-separated with headers?"))
        cols <- names(cols)
      } else cols <- c("", "", "")

      tagList(
        h4(strong("Columns names")),
        fluidRow(
          column(width = 6,
                 selectizeInput(
                   NS(id, "col_date"), width = "100%",
                   label = HTML("Date column"),
                   choices = cols, selected = cols[1])),
          column(width = 6,
                 selectizeInput(
                   NS(id, "col_value"),
                   label = HTML("Flow column"),
                   choices = cols, selected = cols[2]))),
        fluidRow(
          column(width = 6,
                 selectizeInput(
                   NS(id, "col_symbol"),
                   label = HTML("Symbols column"),
                   choices = cols, selected = cols[3]))
          ))
    })

    output$ui_stn <- renderUI({

      if(input$source == "HYDAT") {
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
                    label = "Name", value = stn_name,
                    placeholder = "ex. Mission Creek")),
        column(
          width = 6,
            numericInput(NS(id, "basin_area"),
                         label = HTML("Basin area (km<sup>2</sup>)"),
                         value = basin,
                         min = 0, step = 0.1)),
        bsTooltip(NS(id, "station_info"),
                  paste0("Station Name for context<p><p>",
                         tips$basin_area),
                  placement = "left"))
    })

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
               paste0("Specify basin area under ",
                      "Station Information")) %then%

          need(data_id() == input$file$name,
               "Click the Load Data button!"),

        errorClass = "helper")

    })

    output$csv_checks <- renderText({
      msgs <- list()

      if(input$source == "CSV" & !is.null(input$file)) {
        x <- read.csv(input$file$datapath)
        x <- x[[input$col_date]]

        if(any(duplicated(x))) {
          msgs$dups <- paste0("There are duplicate dates in the data... ",
                              "is this from a single station?")
        }
        if("try-error" %in% class(try(as.Date(x), silent = TRUE))) {
          msgs$dates <- "Date column is not in the standard date format 'YYYY-MM-DD'"
        }
      }

      if(input$basin_area == 0) msgs$basin <- "Specify basin area under Station Information"

      validate(need(FALSE, paste0(msgs, collapse = "\n")), errorClass = "red")

    }) %>%
      bindEvent(input$load)

    output$csv_preview <- renderText({
      req(input$file)
      readLines(input$file$datapath, n = 20) %>%
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
        dplyr::slice(sub) %>%
        dplyr::mutate(selected = .data$STATION_NUMBER == input$station_number) %>%
        dplyr::arrange(selected)
    })

    # HYDAT Map -------------------------

    pal <- leaflet::colorNumeric(c("#31688E", "red"), c(FALSE, TRUE))

    add_markers <- function(map, data) {
      leaflet::addCircleMarkers(
        map, data = data,
        group = "points",
        options = leaflet::pathOptions(pane = "points"),
        lng = ~LONGITUDE, lat = ~LATITUDE,
        layerId = ~STATION_NUMBER,
        radius = 6, fillOpacity = 1, stroke = TRUE,
        fillColor = ~pal(selected),
        color = "black", opacity = 1, weight = 1,
        label = ~purrr::map(glue::glue(
          "<strong>{stringr::str_to_title(STATION_NAME)}</strong><br>",
          "<strong>Station ID</strong>: {STATION_NUMBER}<br>",
          "<strong>Status</strong>: {HYD_STATUS}<br>",
          "<strong>Year range</strong>: {Year_from}-{Year_to}<br>",
          "<strong>No. Years</strong>: {RECORD_LENGTH}"), HTML))
    }

    output$hydat_map <- leaflet::renderLeaflet({

      l <- leaflet::leaflet() %>%

        # base maps
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addProviderTiles(
          leaflet::providers$Stamen.Terrain, group = "Stamen (Terrain)") %>%

        # Map panes for vertical sorting
        leaflet::addMapPane("points", zIndex = 430) %>%
        leaflet::addMapPane("polygons", zIndex = 410) %>%

        # Stations
        add_markers(data = stations()) %>%

        # Controls
        leaflet::addLayersControl(
          baseGroups = c("Stamen (Terrain)", "OpenStreetMap"),
          overlayGroups = bc_maps_labs$group,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )

      # Hide all polygons except the first one
      for(i in bc_maps_labs$group[-1]) l <- leaflet::hideGroup(l, i)

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
      leaflet::leafletProxy("hydat_map") %>%
        leaflet::clearGroup("points") %>%
        add_markers(data = stations_sub())
    }) %>%
      bindEvent(stations_sub())



    # HYDAT Table ------------------------
    output$hydat_table <- DT::renderDT({
      stations() %>%
        DT::datatable(selection = "single", rownames = FALSE, filter = 'top',
                      extensions = c("Scroller"),
                      options = list(scrollX = TRUE,
                                     scrollY = 450, deferRender = TRUE,
                                     scroller = TRUE,
                                     dom = 'Brtip'))
    })

    # Raw data ------------------
    data_raw <- reactive({
      req(input$water_year, input$load > 0)

      if (input$source == "HYDAT") {
        req(input$station_number)

        d1 <- glue::glue(
          "data_flow <- fill_missing_dates(",
          "        station_number = '{input$station_number}')")

        d2 <- d1

        # Set data info
        data_type("HYDAT")
        data_id(input$station_number)

      } else {
        req(input$file, input$col_date, input$col_value, input$col_symbol,
            input$basin_area != 0)

        validate(need(length(unique(c(
          input$col_date, input$col_value, input$col_symbol))) == 3,
          "Date, Value and Symbol columns must be distinct"))

        validate(need(!is.null(input$basin_area) && input$basin_area > 0,
                      paste0("Must have a valid, non-zero basin area ",
                             "(see Station Information)")))

        d <- glue::glue(
          "dplyr::rename(Date = {input$col_date}, Value = {input$col_value}, ",
          "              Symbol = {input$col_symbol}) %>%",
          "fill_missing_dates()")

        # For user to reproduce locally
        d1 <- glue::glue("data_flow <- read.csv('{input$file$name}') %>% ", d)

        # Real loading
        d2 <- glue::glue("data_flow <- read.csv('{input$file$datapath}') %>% ",
                         d)

        data_type("CSV")
        data_id(basename(input$file$name))
      }

      d_dates <- glue::glue(
        " %>% ",
        "add_date_variables(water_year_start = {as.numeric(input$water_year)}) %>%",
        "add_daily_volume() %>%",
        "add_daily_yield(basin_area = {input$basin_area})")

      code$data_raw <- glue::glue(d1, d_dates)
      labels$data_raw <- "Load data, prep dates, calculate volume/yield"

      data_loaded(TRUE)
      eval_check(glue::glue(d2, d_dates))

    }) %>%
      bindEvent(input$load, input$water_year,
                ignoreInit = TRUE)

    # Plot ----------------
    output$plot <- plotly::renderPlotly({
      check_data(data_loaded())
      req(input$daterange,
          input$years_range,
          input$water_year)

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_flow_data", data_name = "data_flow", input)

      code$data_plot <- g
      labels$data_plot <- "Plot general flows"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(data_settings(), "Flow")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      g %>%
        plotly::ggplotly() %>%
        plotly::config(modeBarButtonsToRemove =
                 c("pan", "autoscale", "zoomIn2d", "zoomOut2d",
                   "hoverCompareCartesian", "hoverClosestCartesian"))
    })

    # Table ----------------
    output$table <- DT::renderDT({
      check_data(data_loaded())
      req(input$years_range)
      data_raw() %>%
        dplyr::select(-"Month") %>%
        dplyr::filter(.data$WaterYear >= input$years_range[1],
                      .data$WaterYear <= input$years_range[2],
                      !.data$WaterYear %in% input$years_exclude,
                      .data$MonthName %in% month.abb[as.numeric(input$months)]) %>%
        prep_DT()
    })

    # R Code ----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels,
                                          order = c("data_raw", "data_plot")))


    # Sidebar: Data Info ------------------------
    output$info <- gt::render_gt({

      d <- dplyr::tibble(name = "", value = "", .rows = 0)
      t <- "Current Data: None"
      s <- NULL

      if(!is.null(input$water_year) & !is.null(input$years_range) &
         !is.null(input$months)) {

        if(is.null(input$years_exclude)) {
          ye <- ""
        } else ye <- conseq(input$years_exclude, wrap = FALSE)

        t <- glue::glue("Current Data: {data_id()}")
        s <- input$station_name

        m <- input$months
        if(all(1:12 %in% m)) m <- "all" else m <- conseq(m, type = "month")

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
                  `Total Years` = glue::glue(
                    "{n} / {input$years_range[2] - input$years_range[1] + 1}"),
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
    data_settings <- reactive({
      list("discharge" = input$discharge,
           "water_year" = input$water_year,
           "years_range" = input$years_range,
           "years_exclude" = input$years_exclude,
           "months" = input$months,
           "roll_days" = input$roll_days,
           "roll_align" = input$roll_align,
           "complete" = input$complete,
           "missing" = input$missing,
           "allowed" = input$allowed,
           "basin_area" = input$basin_area,
           "station_name" = input$station_name)
    })

    list(
      data_settings = data_settings,
      data_raw = data_raw,
      data_loaded = data_loaded,
      data_code = reactiveVal(code$data_raw))
  })
}

