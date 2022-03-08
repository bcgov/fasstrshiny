# Copyright 2021 Province of British Columbia
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

server <- function(input, output, session) {

  # Global reactives ----------------------------------
  code <- reactiveValues()                # Holds code

  meta <- reactiveValues(station_id = "",
                         station_name = "",
                         basin_area = NA_real_)

  # UI elements ---------------------------------------

  ## Data ----------------------
  output$ui_data_water_year <- renderUI({
    tagList(
      radioGroupButtons(
        "data_water_year",
        label = "Water year start",
        choices = setNames(1:12, month.abb),
        selected = 1, size = "sm", width = "100%"),
      bsTooltip("data_water_year",
                title = tips$water_year, placement = "left"))
  })

  output$ui_data_years_range <- renderUI({
    req(data_raw())


    yr_orig <- c(min(data_raw()$WaterYear), max(data_raw()$WaterYear))
    yr <- input$data_years_range
    if(is.null(yr)) {
      yr <- yr_orig
    } else {
      if(yr[1] < yr_orig[1]) yr[1] <- yr_orig[1]
      if(yr[2] > yr_orig[2]) yr[2] <- yr_orig[2]
    }

    tagList(
      sliderInput("data_years_range",
                  label = "Start and end years",
                  min = min(data_raw()$WaterYear),
                  max = max(data_raw()$WaterYear),
                  value = yr,
                  dragRange = TRUE, sep = ""),
      bsTooltip("data_years_range", title = tips$years_range,
                placement = "left"))
  })

  output$ui_data_years_exclude <- renderUI({
    req(input$data_years_range)

    # If updating, use old values where within range
    isolate({
      if(!is.null(input$data_years_exclude)) {
        s <- as.numeric(input$data_years_exclude)
        s <- s[s >= input$data_years_range[1] & s <= input$data_years_range[2]]
      } else s <- NULL
    })

    tagList(
      selectizeInput("data_years_exclude",
                     label = "Years to exclude",
                     choices = seq(from = input$data_years_range[1],
                                   to = input$data_years_range[2], by = 1),
                     selected = s,
                     multiple = TRUE),
      bsTooltip("data_years_exclude", title = tips$years_exclude,
                placement = "left"))
  })

  output$ui_data_months <- renderUI({
    req(input$data_water_year)

    # Arrange months by water year
    m <- setNames(1:12, month.abb)
    m <- c(m[m >= as.numeric(input$data_water_year)],
           m[m < as.numeric(input$data_water_year)])

    tagList(
      selectizeInput("data_months",
                     label = "Months to Include",
                     choices = m,
                     selected = 1:12,
                     multiple = TRUE),
      bsTooltip("data_months", tips$months, placement = "left"))
  })

  # Update station from Map button
  observe({
    updateTextInput(session, "data_station_num",
                    value = input$data_hydat_map_marker_click$id)
  }) %>%
    bindEvent(input$data_hydat_map_marker_click)

  # Update station from Table button
  observe({
    updateTextInput(
      session, "data_station_num",
      value = stations$station_number[input$data_hydat_table_rows_selected])
  }) %>%
    bindEvent(input$data_hydat_table_rows_selected)

  # Hide/Show based on toggle
  observe(toggle("data_stn", condition = input$data_show_stn))
  observe(toggle("data_dates", condition = input$data_show_dates))
  observe(toggle("data_types", condition = input$data_show_types))


  # Add plot options as Gear in corner
  output$ui_data_plot_options <- renderUI({
    req(data_raw())

    select_plot_options(
      select_plot_log("data", value = formals(plot_flow_data)$log_discharge), # Default
      select_daterange("data", data_raw()))
  })

  ##  Hydro graphs -------------------------------------------------------------
  output$ui_hydro <- renderUI({
    build_ui(id = "hydro", input, include = "complete")
  })

  output$ui_hydro_monthly_plot <- renderUI({
    req(input$hydro_type == "Monthly")

    p <- c("Mean", "Median", "Maximum", "Minimum")
    if(!is.null(input$hydro_percentiles)) p <- c(p, glue("P{input$hydro_percentiles}"))

    selectizeInput("hydro_monthly_plot", label = "Statistic to plot",
                   choices = p, selected = 1)

  })

  # Plot options
  output$ui_hydro_plot_options <- renderUI({
    id <- "hydro"
    select_plot_options(
      select_plot_log(id,
                      value = formals(plot_longterm_daily_stats)$log_discharge),
      select_add_year(id, input),
      select_add_dates(id),
      select_add_mad(id))
    # Add inner/outer percentiles?
  })


  # Enable/disable based on type
  observe({
    # add year
    if(input$hydro_type %in% c("Long-term", "Daily")) {
      enable("hydro_add_year")
    } else {
      disable("hydro_add_year")
    }

    # add dates
    if(input$hydro_type == "Daily") {
      enable("hydro_add_dates")
    } else {
      disable("hydro_add_dates")
    }

    # custom months
    if(input$hydro_type == "Long-term") {
      enable("hydro_custom_months")
      enable("hydro_custom_months_label")
    } else {
      disable("hydro_custom_months")
      disable("hydro_custom_months_label")
    }

  }) %>%
    bindEvent(input$hydro_type)

  # Table options
  output$ui_hydro_table_options <- renderUI({
    select_table_options(id = "hydro", input)
  })

  ## Flows ----------------------------------------------------------
  output$ui_flows <- renderUI({
    build_ui(id = "flows", input,
             include = c("complete", "custom_months"))
  })

  # Plot options
  output$ui_flows_plot_options <- renderUI({
    select_plot_options(
      select_plot_log(id = "flows",
                      value = formals(plot_flow_duration)$log_discharge))
  })


  ## Cumulative ----------------------------------------------------------

  output$ui_cum_plot_options <- renderUI({
    id <- "cum"
    select_plot_options(
      select_plot_log(
        id, value = formals(plot_daily_cumulative_stats)$log_discharge),
      select_add_year(id, input))
  })

  # Table options
  output$ui_cum_table_options <- renderUI({
    select_table_options(data = data_raw(), id = "cum", input,
                         include = "percentiles")
  })


  ## Annual Trends ------------------------------------------------

  # Excluded years, takes defaults from input$data_years_exclude,
  # but allowed to modify here
  output$ui_at_exclude <- renderUI({
    req(input$data_years_range)
    tagList(
      selectizeInput("at_years_exclude",
                     label = "Years to exclude",
                     choices = seq(from = input$data_years_range[1],
                                   to = input$data_years_range[2], by = 1),
                     selected = input$data_years_exclude,
                     multiple = TRUE),
      bsTooltip(id = "at_years_exclude", title = tips$years_exclude,
                placement = "left"))
  })

  # Update years_exclude as points selected/unselected
  observe({
    updateNumericInput(inputId = "at_years_exclude",
                       value = c(at_excluded(),
                                 input$at_plot_selected))
  }) %>%
    bindEvent(input$at_plot_selected)

  output$ui_at_allowed <- renderUI({
    tagList(
      sliderInput("at_allowed_annual",
                  label = "Annual - Allowed missing (%)",
                  value = input$data_allowed, step = 5, min = 0, max = 100),
      sliderInput("at_allowed_monthly",
                  label = "Monthly - Allowed missing (%)",
                  value = input$data_allowed, step = 5, min = 0, max = 100),
      bsTooltip("at_allowed_annual", tips$allowed, placement = "left"),
      bsTooltip("at_allowed_monthly", tips$allowed, placement = "left")
      )
  })



  ## Volume Frequency ----------------------------------------

  # Excluded years, takes defaults from input$data_years_exclude,
  # but allowed to modify here
  output$ui_vf_exclude <- renderUI({
    req(input$data_years_range)
    selectizeInput("vf_years_exclude",
                   label = "Years to exclude",
                   choices = seq(from = input$data_years_range[1],
                                 to = input$data_years_range[2], by = 1),
                   selected = input$data_years_exclude,
                   multiple = TRUE)
  })

  # Update years_exclude as points selected/unselected
  observe({
    updateNumericInput(inputId = "vf_years_exclude",
                       value = c(vf_excluded(),
                                 input$vf_plot_selected))
  }) %>%
    bindEvent(input$vf_plot_selected)

  output$ui_vf_day <- renderUI({
    req(vf_freqs())
    radioGroupButtons("vf_day",
                      choices = names(vf_freqs()$Freq_Fitting),
                      selected = names(vf_freqs()$Freq_Fitting)[1])
  })


  # Data - Loading ---------------

  ## HYDAT Map -------------------------
  output$data_hydat_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(
        data = bc_hydrozones,
        stroke = 0.5, opacity = 1, weight = 1,
        fillOpacity = 0.15, fillColor = "black", color = "black",
        label = ~str_to_title(HYDROLOGICZONE_NAME)) %>%
      addCircleMarkers(
        data = stations, lng = ~longitude, lat = ~latitude,
        layerId = ~ station_number,
        radius = 3, fillOpacity = 1, stroke = FALSE, color = "#31688E",
        label = ~ station_number,
        popup = ~glue("<strong>Station Name:</strong> ",
                      "{stringr::str_to_title(station_name)}<br>",
                      "<strong>Station Number:</strong> {station_number}"))
  })

  ## HYDAT Table ------------------------
  output$data_hydat_table <- renderDT({
    stations %>%
      select("station_number", "station_name", "province",
             "hyd_status", "real_time", "regulated", "parameters") %>%
      datatable(selection = "single", rownames = FALSE, filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE,
                               scrollY = 450, deferRender = TRUE,
                               scroller = TRUE,
                               dom = 'Brtip'))
  })

  ## Raw data ------------------
  data_raw <- reactive({
    req(input$data_station_num, input$data_water_year,
        input$data_load > 0)

    if (input$data_source == "HYDAT") {
      m <- filter(stations, .data$station_number == input$data_station_num)
      meta$station_id <- input$data_station_num
      meta$station_name <- m$station_name
      meta$basin_area <- as.numeric(m$drainage_area_gross)

      d <- glue(
        "data_flow <- fill_missing_dates(",
        "        station_number = '{input$data_station_num}') %>%",
        "  add_date_variables(water_year_start = {as.numeric(input$data_water_year)}) %>%",
        "  add_daily_volume() %>%",
        "  add_daily_yield()")

    } else {
      inFile <- input$data_file
      if (is.null(inFile)) return(NULL)

      meta$station_id <- basename(input$data_file)
      meta$station_name <- input$data_station_name
      meta$basin_area <- as.numeric(input$data_basin_area)

      d <- glue("data_flow <- read.csv({inFile$datapath}) %>%",
                "  fill_missing_dates() %>%",
                "  add_date_variables(water_year_start = {as.numeric(input$data_water_year)})")

    }

    # Save unevaluated code for code tab
    code$data_raw <- d

    eval(parse(text = d)) # Evaluate and create data_flow
  }) %>%
    bindEvent(input$data_load, input$data_water_year,
              ignoreInit = TRUE)

  ## Plot ----------------
  output$data_plot <- renderPlotly({
    check_data(input)
    req(data_raw(),
        !is.null(input$data_plot_log),
        input$data_daterange,
        input$data_years_range,
        !is.null(input$data_water_year))

    data_flow <- data_raw()

    g <- create_fun(
      "plot_flow_data", "data_flow", id = "data", input,
      params = c("plot_log", "daterange"))

    code$data_plot <- g

    parse(text = g) %>%
      eval() %>%
      .[[1]] %>%
      ggplotly() %>%
      config(modeBarButtonsToRemove =
               c("pan", "autoscale", "zoomIn2d", "zoomOut2d",
                 "hoverCompareCartesian", "hoverClosestCartesian"))
  })

  ## Table ----------------
  output$data_table <- renderDT({
    req(input$data_years_range)

    check_data(input)
    data_raw() %>%
      rename("StationNumber" = "STATION_NUMBER") %>%
      select(-"Month") %>%
      filter(WaterYear >= input$data_years_range[1],
             WaterYear <= input$data_years_range[2],
             !WaterYear %in% input$data_years_exclude,
             MonthName %in% month.abb[as.numeric(input$data_months)]) %>%
      mutate(Value = round(Value, 4)) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Bfrtip'))
  })

  ## R Code ----------------
  output$data_code <- renderText({
    fasstrshiny:::code_format(code, id = "data")
  })


  ## Sidebar: Data Info ------------------------
  output$data_info <- render_gt({

    d <- tibble(name = "", value = "", .rows = 0)
    t <- "Current Data: None"
    s <- NULL

    if(!is.null(input$data_water_year) & !is.null(input$data_years_range) &
       !is.null(input$data_months)) {

      if(is.null(input$data_years_exclude)) {
        ye <- ""
      } else ye <- glue_collapse(input$data_years_exclude, sep = ", ")

      t <- glue("Current Data: {meta$station_id}")
      s <- meta$station_name

      m <- input$data_months
      if(all(1:12 %in% m)) m <- "all" else m <- glue_collapse(m, sep = ", ")

      n <- data_raw() %>%
        filter(WaterYear >= input$data_years_range[1],
               WaterYear <= input$data_years_range[2],
               !WaterYear %in% input$data_years_exclude) %>%
        pull(WaterYear) %>%
        unique() %>%
        length()


      d <- list(`Water Year` = month.abb[as.numeric(input$data_water_year)],
                `Year Range` = glue_collapse(input$data_years_range, sep = "-"),
                `Years Excl.` = ye,
                `Total Years` = as.character(n),
                `Months` = m) %>%
        tibble::enframe() %>%
        unnest(value)
    }

    gt(d) %>%
      cols_align("left") %>%
      cols_width(name ~ px(65)) %>%
      tab_header(title = t, subtitle = s) %>%
      tab_options(column_labels.hidden = TRUE,
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
      tab_style(
        style = cell_borders(
          sides = c("top", "bottom"),
          weight = 0),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))
  })



  # Data - Availability ---------------
  ## Data --------------
  available_raw <- reactive({
    req(data_raw())

    data_flow <- data_raw()
    d <- glue("
      data_available <- screen_flow_data(
        data = data_flow,
        start_year = {input$data_years_range[1]},
        end_year = {input$data_years_range[2]},
        water_year_start = {as.numeric(input$data_water_year)})
        ")

    code$available_data <- d
    eval(parse(text = d))
  })

  ## Summary plot ------------------
  output$available_plot1 <- renderGirafe({
    check_data(input)
    req(available_raw())

    xlab <- if_else(input$data_water_year != 1, "Water Year", "Year")
    title <- paste0("Annual Daily ", input$available_summary, " - ", meta$station_name)
    data_available <- available_raw()

    g <- glue(
      "ggplot(data = data_available,
              aes(x = Year, y = {input$available_summary})) +
        theme_bw() +
        theme(axis.title = element_text(size = 15),
              plot.title = element_text(size = 15, hjust = 0.5),
              axis.text = element_text(size = 13)) +
        geom_line(colour = 'dodgerblue4') +
        geom_point(colour = 'firebrick3', size = 2) +
        labs(x = '{xlab}', y = 'Discharge (cms)', title = '{title}')")

    code$available_plot <- g

    g <- parse(text = g) %>%
      eval() +
      geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n",
                         input$available_summary, ": ",
                         round(.data[[input$available_summary]], 4)),
        data_id = Year),
        colour = 'firebrick3', size = 3)

    girafe(ggobj = g, width_svg = 13, height_svg = 4.5)
  })


  ## Missing Data Plot ---------------------------
  output$available_plot2 <- renderGirafe({
    check_data(input)
    req(data_raw())

    data_flow <- data_raw()
    g <- create_fun(
      "plot_missing_dates", data = "data_flow",
      id = "available", input,
      params_ignore = "months",
      extra = glue("months = c({glue_collapse(input$available_months, sep = ', ')})"))

    code$available_miss <- g

    g <- parse(text = g) %>%
      eval() %>%
      .[[1]]

    # Replace layers with interactive
    g$layers[[1]] <- geom_bar_interactive(
      aes(tooltip = paste0("Year: ", Year, "\nMissing: ", Value),
          data_id = paste0(Year, "-", Month)), colour = "cornflowerblue",
      fill = "cornflowerblue", stat = "identity")

    girafe(ggobj = g, width_svg = 14, height_svg = 6,
           options = list(opts_selection(type = "none")))
  })

  ## Summary table ------------------
  output$available_table <- DT::renderDT({
    check_data(input)
    available_raw() %>%
      select(-dplyr::contains("STATION_NUMBER")) %>%
      rename("Total days" = "n_days",
             "Total flow days" = "n_Q",
             "Total missing days" = "n_missing_Q",
             "Standard deviation" = "StandardDeviation") %>%
      rename_with(.cols = ends_with("_missing_Q"),
                  ~ str_replace(., "_missing_Q", " missing days")) %>%
      mutate_if(is.numeric, ~round(., 4)) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE,
                               scrollY = 450, deferRender = TRUE,
                               scroller = TRUE,
                               dom = 'Brtip'))
  })

  ## R Code -----------------
  output$available_code <- renderText({
    fasstrshiny:::code_format(code, id = "available")
  })


  # Overview ------------------------------------------------------








  # Hydrographs ---------------------------------------

  ## Plot --------------------
  output$hydro_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$hydro_type,
        !is.null(input$hydro_plot_log), !is.null(input$hydro_add_year))

    data_flow <- data_raw()

    if(input$hydro_add_year != "") {
      e <- glue("add_year = {input$hydro_add_year}")
    } else e <- NULL

    g <- switch(input$hydro_type,
                "Daily" = "plot_daily_stats",
                "Long-term Monthly" = "plot_longterm_monthly_stats",
                "Long-term Daily" = "plot_longterm_daily_stats") %>%
      create_fun("data_flow", id = "hydro", input,
                 params = c("plot_log", "complete", "missing"),
                 extra = e)

    code$hydro_plot <- g
    g <- eval(parse(text = g))[[1]]

    # Add interactivity
    if(input$hydro_type == "Long-term") {
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("LT_Mean", "LT_Med")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Year_mean"] <- input$hydro_add_year

      # Add interactive vline
      g <- g + fasstrshiny:::create_vline_interactive(
        data = g$data, stats = stats, size = 20)

    } else if(input$hydro_type == "Daily") {
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("DayofYear", "AnalysisDate")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Date"] <- "Day"
      names(stats)[stats == "RollingValue"] <- input$hydro_add_year

      # Add Interactive vline
      g <- g + fasstrshiny:::create_vline_interactive(data = g$data, stats)
    }

    # Add dates
    if(input$hydro_type == "Daily" & !is.null(input$hydro_add_dates)){
      dts <- data.frame(
        Date = fasstrshiny:::get_date(
          input$hydro_add_dates,
          water_year = as.numeric(input$data_water_year))) %>%
        mutate(labs = format(Date, '%b-%d'),
               hjust = if_else(as.numeric(input$data_water_year) ==
                                 as.numeric(format(Date, "%m")),
                               -0.05, 1.05))

      g <- g +
        geom_vline_interactive(xintercept = dts$Date, colour = 'grey20',
                               tooltip = dts$labs) +
        geom_text(data = dts, aes(x = Date, label = labs, hjust = hjust),
                  y = Inf, vjust = 2)
    }

    # Add mad
    if(!is.null(input$hydro_add_mad) && input$hydro_add_mad) {

      mad <- hydro_mad() %>%
        pivot_longer(-STATION_NUMBER, names_to = "type")

      g <- g +
        geom_hline(data = mad,
                   aes(yintercept = value),
                   size = c(2, rep(1, nrow(mad) - 1))) +
        geom_hline_interactive(
          data = mad,
          aes(tooltip = paste0(str_replace(type, "%", "% "),
                               ": ", round(value, 4)),
              yintercept = value), alpha = 0.01,
          size = 3) +
        geom_text(data = mad, aes(y = value, label = type),
                  x = c(Inf, rep(-Inf, nrow(mad) - 1)), colour = NA,
                  hjust = c(1.1, rep(-0.1, nrow(mad) -1)), vjust = -0.5)
    }

    girafe(ggobj = g, width_svg = 12, height = 6,
           options = list(
             opts_toolbar(position = "topleft"),
             opts_selection(type = "none"),
             opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
  })



  ## MAD -----------------------
  hydro_mad <- reactive({
    req(input$hydro_discharge, input$hydro_mad)

    data_flow <- data_raw()

    t <- create_fun(
      fun = "calc_longterm_mean",
      data = "data_flow", id = "hydro", input,
      params = "complete",
      extra = glue("percent_MAD = c({glue_collapse(input$hydro_mad, sep = ',')})"))

    code$hydro_mad <- t

    parse(text = t) %>%
      eval()
  })

  ## Table -----------------------
  output$hydro_table <- DT::renderDT({
    check_data(input)
    req(input$hydro_type, input$hydro_discharge)

    data_flow <- data_raw()

    p <- "percentiles"

    p <- switch(input$hydro_type,
                "Long-term" = c("complete", "missing", "custom_months",
                                "custom_months_label"),
                "Annual" = "allowed",
                "Monthly" = "allowed",
                "Daily" = "complete", "missing")

    p <- c(p, "percentiles")

    t <- switch(input$hydro_type,
                "Long-term" = "calc_longterm_daily_stats",
                "Annual" = "calc_annual_stats",
                "Monthly" = "calc_monthly_stats",
                "Daily" = "calc_daily_stats") %>%
      create_fun("data_flow", id = "hydro", input,
                 params = p)

    code$hydro_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 500, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$hydro_code <- renderText({
    fasstrshiny:::code_format(code, id = "hydro")
  })


  # Cumulative Hydrographs --------------------------------------------------

  ## Plot --------------------
  output$cum_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$cum_type, !is.null(input$cum_add_year))

    data_flow <- data_raw()

    e <- glue("use_yield = {input$cum_discharge}")
    if(input$cum_add_year != "") {
      e <- glue("{e}, add_year = {input$cum_add_year}")
    }


    g <- switch(input$cum_type,
                "Monthly" = "plot_monthly_cumulative_stats",
                "Daily"   = "plot_daily_cumulative_stats") %>%
      create_fun("data_flow",
                 id = "cum", input, params = "plot_log",
                 params_ignore = c("discharge", "roll_days", "roll_align"),
                 extra = e)

    code$cum_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))[[1]]

    stats <- names(g$data) # Get stats from plot data
    stats <- stats[!stats %in% c("WaterYear", "AnalysisDate", "DayofYear")] # Omit these

    # For tooltips labels...
    names(stats)[stats == "Monthly_Total"] <- input$cum_add_year
    names(stats)[stats == "Cumul_Flow"] <- input$cum_add_year

    # Add interactive vline
    g <- g + fasstrshiny:::create_vline_interactive(
      data = g$data, stats = stats,
      size = if_else(input$cum_type == "Monthly", 20, 1))


    girafe(ggobj = g, width_svg = 14, height_svg = 6,
           options = list(
             opts_toolbar(position = "topleft"),
             opts_selection(type = "none"),
             opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
  })


  ## Table -----------------------
  output$cum_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$cum_discharge)

    data_flow <- data_raw()

    t <- switch(input$cum_type,
                "Monthly" = "calc_monthly_cumulative_stats",
                "Daily"   = "calc_daily_cumulative_stats") %>%
      create_fun("data_flow",
                 id = "cum", input, params = "percentiles",
                 params_ignore = c("discharge", "roll_days", "roll_align"),
                 extra = glue("use_yield = {input$cum_discharge}"))

    code$cum_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$cum_code <- renderText({
    fasstrshiny:::code_format(code, id = "cum")
  })



  # Flows ---------------------------------------

  ## Flow Percentile -----------------------
  output$flows_perc <- renderText({
    req(input$flows_discharge, input$flows_flow)

    data_flow <- data_raw()

    # Flow
    t <- create_fun(fun = "calc_flow_percentile",
                    data = "data_flow", id = "flows", input,
                    params = "complete",
                    extra = glue("flow_value = {input$flows_flow}"))

    code$flows_flow <- t

    parse(text = t) %>%
      eval() %>%
      pull(Percentile) %>%
      round(4)
  })

  ## Plot --------------------
  output$flows_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), !is.null(input$flows_plot_log))

    data_flow <- data_raw()

    # missing arguments
    # - include_longterm

    g <- create_fun(
      fun = "plot_flow_duration", data = "data_flow", id = "flows", input,
      params = c("custom_months", "custom_months_label", "complete",
                 "missing", "plot_log"),
      end = "[[1]]")

    code$flows_plot <- g

    g <- eval(parse(text = g))
    g <- g +
      geom_point_interactive(
        aes(tooltip = paste0("% Time: ", Percentile, "\n",
                             "Month: ", Month),
            data_id = Percentile),
        show.legend = FALSE, alpha = 0.01, size = 3)

    girafe(ggobj = g, width_svg = 12, height = 6,
           options = list(
             opts_toolbar(position = "topleft"),
             opts_selection(type = "none"),
             opts_hover(css = "fill:orange; stroke:gray;fill-opacity:1;")))
  })


  ## Table -----------------------
  output$flows_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$flows_discharge)

    data_flow <- data_raw()

    t <- create_fun(fun = "calc_longterm_daily_stats",
                    data = "data_flow", id = "flows", input,
                    params = c("custom_months", "custom_months_label",
                               "missing"),
                    extra = "percentiles = 1:99",
                    end = "%>% select(-Mean, -Median, -Minimum, -Maximum)")

    code$flows_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 500, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })





  ## R Code -----------------
  output$flows_code <- renderText({
    fasstrshiny:::code_format(code, id = "flows")
  })



  # Annual Statistics -----------------

  # Annual Means ---------------------------------------


  ## Plot --------------------
  output$am_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$am_missing)

    data_flow <- data_raw()

    g <- create_fun(
      fun = "plot_annual_means", data = "data_flow", id = "am", input,
      params = c("missing"),
      end = "[[1]]")

    code$am_plot <- g

    g <- eval(parse(text = g))

    # Replace layers with interactive
    g$layers[[1]] <- geom_bar_interactive(
      aes(tooltip = paste0("Year: ", Year, "\nMAD: ", round(Mean, 4)),
          data_id = Year), colour = "cornflowerblue",
      fill = "cornflowerblue", stat = "identity")

    girafe(ggobj = g, width_svg = 14, height_svg = 4,
           options = list(opts_selection(type = "none")))
  })


  ## R Code -----------------
  output$am_code <- renderText({
    fasstrshiny:::code_format(code, id = "am")
  })






  # Flow timing ---------------------------------------
  ## Plot --------------------
  output$ft_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$ft_percent)

    data_flow <- data_raw()

    g <- create_fun(
      "plot_annual_flow_timing", data = "data_flow",
      id = "ft", input,
      params_ignore = c("roll_days", "roll_align"),
      extra = glue("percent_total = ",
                   "c({glue_collapse(input$ft_percent, sep = ',')})"),
      end = "[[1]]")

    code$ft_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))

    g <- g +
      geom_point_interactive(
        aes(tooltip = paste0("Year: ", Year, "\n",
                             Statistic, "\n",
                             "Day of Year: ", Value),
            data_id = Year), size = 3)

    girafe(ggobj = g, width_svg = 14, height_svg = 6,
           options = list(
             opts_toolbar(position = "topleft"),
             opts_selection(type = "none")))
  })


  ## Table -----------------------
  output$ft_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$ft_percent)

    data_flow <- data_raw()

    t <- create_fun(
      "calc_annual_flow_timing", data = "data_flow",
      id = "ft", input,
      params_ignore = c("roll_days", "roll_align"),
      extra = glue("percent_total = ",
                   "c({glue_collapse(input$ft_percent, sep = ',')})"))

    code$ft_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$ft_code <- renderText({
    fasstrshiny:::code_format(code, id = "ft")
  })


  # Low Flows ---------------------------------------
  ## Plot --------------------
  output$lf_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$lf_discharge)

    data_flow <- data_raw()

    g <- create_fun(
      "plot_annual_lowflows", data = "data_flow",
      id = "lf", input,
      params = c("discharge", "allowed"),
      params_ignore = c("roll_days", "roll_align"),
      extra = glue(
        "roll_days = c({glue_collapse(input$lf_roll_days, sep = ',')}), ",
        "roll_align = '{input$lf_roll_align}'"))

    code$lf_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))

    g[["Annual_Low_Flows"]] <- g[["Annual_Low_Flows"]] +
      geom_point_interactive(
        aes(tooltip = paste0("Year: ", Year, "\n",
                             Statistic, "\n",
                             "Discharge: ", round(Value, 4)),
            data_id = Year), size = 3)

    g[["Annual_Low_Flows_Dates"]] <- g[["Annual_Low_Flows_Dates"]] +
      geom_point_interactive(
        aes(tooltip = paste0("Year: ", Year, "\n",
                             Statistic, "\n",
                             "Day of Year: ", round(Value, 4)),
            data_id = Year), size = 3)

    # Combine plots
    g <- wrap_plots(g)

    girafe(ggobj = g, width_svg = 12, height_svg = 6,
           options = list(
             opts_toolbar(position = "topleft"),
             opts_selection(type = "none")))
  })


  ## Table -----------------------
  output$lf_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$lf_discharge)

    data_flow <- data_raw()

    t <- create_fun(
      "calc_annual_lowflows", data = "data_flow",
      id = "lf", input,
      params = c("discharge", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$lf_roll_days, sep = ',')})"))

    code$lf_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$lf_code <- renderText({
    fasstrshiny:::code_format(code, id = "lf")
  })

  # Peak Flows ---------------------------------------

  ## Table -----------------------
  output$pf_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$pf_discharge)

    data_flow <- data_raw()

    t <- create_fun(
      "calc_annual_peaks", data = "data_flow",
      id = "pf", input,
      params = c("discharge", "missing", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$pf_roll_days, sep = ',')})"))

    code$pf_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$pf_code <- renderText({
    fasstrshiny:::code_format(code, id = "pf")
  })

  # Days outside normal ---------------------------------------
  ## Plot --------------------
  output$on_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$on_normal)

    data_flow <- data_raw()

    g <- create_fun(
      "plot_annual_outside_normal", data = "data_flow",
      id = "on", input,
      extra = glue("normal_percentiles = ",
                   "c({glue_collapse(input$on_normal, sep = ',')})"),
      end = "[[1]]")

    code$on_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))

    g <- g + geom_point_interactive(
      aes(tooltip = paste0("Year: ", Year, "\n",
                           Statistic, "\n",
                           "No. Days: ", round(Value, 4)),
          data_id = Year), size = 3)

    girafe(ggobj = g, width_svg = 12, height_svg = 6,
           options = list(
             opts_toolbar(position = "topleft"),
             opts_selection(type = "none"),
             opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
  })


  ## Table -----------------------
  output$on_table <- DT::renderDT({
    req(data_raw(), input$on_normal)

    data_flow <- data_raw()

    t <- create_fun(
      "calc_annual_outside_normal", data = "data_flow",
      id = "on", input,
      params = "discharge",
      extra = glue("normal_percentiles = ",
                   "c({glue_collapse(input$on_normal, sep = ',')})"))

    code$on_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$on_code <- renderText({
    fasstrshiny:::code_format(code, id = "on")
  })



# Annual Trends -----------------------------------------------
  # Includes low flows, flow timing, and outside normal
  # (also monthly stats/annual stats)


  ## Excluded ----------------------------
  # What years were excluded when the trends were last calculated?
  at_excluded <- reactive({
    input$at_years_exclude
  }) %>%
    bindEvent(input$at_compute)

  ## Trends -----------------------
  at_trends <- reactive({
    req(data_raw(), input$at_zyp)

    data_flow <- data_raw()

    # ingore_missing / allowed missing
    #basin area?

    # Define parameters
    p <- c(
      glue("exclude_years = c({glue_collapse(input$at_years_exclude, sep = ', ')})"),
      glue("zyp_method = '{input$at_zyp}'"),
      glue("annual_percentiles = c({glue_collapse(input$at_annual_percentiles, sep = ', ')})"),
      glue("monthly_percentiles = c({glue_collapse(input$at_monthly_percentiles, sep = ', ')})"),
      glue("stats_days = {input$data_roll_days}"),
      glue("stats_align = '{input$data_roll_align}'"),
      glue("lowflow_days = c({glue_collapse(input$at_low_roll_days, sep = ', ')})"),
      glue("lowflow_align = '{input$at_low_roll_align}'"),
      glue("timing_percent = c({glue_collapse(input$at_percent, sep = ', ')})"),
      glue("normal_percentiles = c({glue_collapse(input$at_normal, sep = ', ')})"),
      glue("allowed_missing_annual = {input$at_allowed_annual}"),
      glue("allowed_missing_monthly = {input$at_allowed_monthly}"),
      glue("zyp_alpha = {input$at_alpha}")) %>%
      glue_collapse(sep = ", ")

    r <- create_fun(
      "compute_annual_trends",
      data = "data_flow", id = "at", input,
      params_ignore = c("roll_days", "roll_align", "years_exclude"),
      extra = p)

    code$at_data <- r

    eval(parse(text = r))
  }) %>%
    bindEvent(input$at_compute)

  ## Table - Fit -----------------------
  output$at_table_fit <- DT::renderDT({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$at_compute,
             "Choose your settings and click 'Compute Trends'"))

    req(at_trends())

    isolate({
      s <- input$at_table_fit_rows_selected
      if(is.null(s)) s <- 1
    })

    at_trends()[["Annual_Trends_Results"]] %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller"),
        options = list(scrollX = TRUE, scrollY = 250, scroller = TRUE,
                       deferRender = TRUE, dom = 'Brtip'),
        selection = list(target = "row", mode = "single", selected = s))
  })

  ## Stat - to plot ---------------------
  at_stat <- reactive({
    req(input$at_table_fit_rows_selected)
    at_trends()[["Annual_Trends_Results"]] %>%
      slice(input$at_table_fit_rows_selected) %>%
      pull(Statistic) %>%
      as.character()
  })

  ## Plot --------------------
  output$at_plot <- renderGirafe({

    g <- at_trends()[[at_stat()]] +
      geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n",
                         at_stat(), ": ", round(Value, 4)),
        data_id = Year), size = 4)

    girafe(ggobj = g, width_svg = 7, height_svg = 5,
           options = list(opts_selection(type = "multiple"),
                          opts_toolbar(position = "topleft")))
  })

  # Add/Remove selected points if changing the numericInput
  observe({
    yrs <- input$at_years_exclude       # All excluded years
    yrs <- yrs[!yrs %in% at_excluded()] # Not ones excluded in last run (point doesn't exist)
    if(length(yrs) == 0) yrs <- NULL

    if(!identical(yrs, input$at_plot_selected)) {
      if(is.null(yrs)) yrs <- ""
      session$sendCustomMessage(type = 'at_plot_set', message = yrs)
    }
  }) %>%
    bindEvent(input$at_years_exclude, ignoreNULL = FALSE, ignoreInit = TRUE)



  ## Table - years sub -----------------------
  output$at_table_years_sub <- render_gt({
    req(at_trends(), at_stat())

    at_trends()[[1]] %>%
      filter(Statistic == at_stat()) %>%
      select(-any_of(c("STATION_NUMBER", "Statistic"))) %>%
      pivot_longer(cols = everything(),
                   names_to = "Year",
                   values_to = at_stat()) %>%
      gt() %>%
      fmt_number(-Year, decimals = 4)
  }, height = px(400))

  ## Table - years -----------------------
  output$at_table_years <- renderDT({

    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$at_compute,
             "Choose your settings and click 'Compute Trends'"))

    req(at_trends())

    at_trends()[[1]] %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })


  ## R Code -----------------
  output$at_code <- renderText({
    fasstrshiny:::code_format(code, id = "at")
  })



  # Volume Frequency - High/Low --------------------------------------------

  ## Excluded ----------------------------
  # What years were excluded when the trends were last calculated?
  vf_excluded <- reactive({
    input$vf_years_exclude
  }) %>%
    bindEvent(input$vf_compute)

  ## Frequencies -----------------------
  vf_freqs <- reactive({
    req(data_raw(), input$vf_use_max)

    validate(need(all(!is.na(fasstrshiny:::text_to_num(input$vf_prob_scale))),
                  "Probabilies to plot must be a comma separated list of numbers"))

    data_flow <- data_raw()

    # Define parameters
    p <- c(
      glue("exclude_years = c({glue_collapse(input$vf_years_exclude, sep = ', ')})"),
      glue("use_max = {input$vf_use_max}"),
      glue("use_log = {input$vf_log}"),
      glue("roll_days = c({glue_collapse(input$vf_roll_days, sep = ', ')})"),
      glue("roll_align = '{input$vf_roll_align}'"),
      glue("prob_plot_position = '{input$vf_prob_plot}'"),
      glue("prob_scale_points = c({input$vf_prob_scale})"),
      glue("fit_distr = '{input$vf_fit_distr}'"),
      glue("fit_quantiles = c({glue_collapse(input$vf_fit_quantiles, sep = ', ')})"),
      glue("plot_curve = {input$vf_plot_curve}")) %>%
      glue_collapse(sep = ", ")

    r <- create_fun(
      "compute_annual_frequencies",
      data = "data_flow", id = "vf", input,
      params = c("discharge", "allowed"),
      params_ignore = c("roll_days", "roll_align", "years_exclude"),
      extra = p)

    code$vf_data <- r

    eval(parse(text = r))
  }) %>%
    bindEvent(input$vf_compute)

  ## Plot --------------------
  output$vf_plot <- renderGirafe({

    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$vf_compute,
             "Choose your settings and click 'Compute Analysis'"))

    g <- vf_freqs()[["Freq_Plot"]] +
      geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n",
                         "Discharge", ": ", round(Value, 4), "\n",
                         "Probability", ": ", round(prob, 4)),
        data_id = Year), size = 3) +
      scale_colour_viridis_d(end = 0.8)

    girafe(ggobj = g, width_svg = 8, height_svg = 5,
           options = list(opts_selection(type = "multiple"),
                          opts_toolbar(position = "topleft")))
  })

  # Remove selected points if changing the numericInput
  observe({
      yrs <- input$vf_years_exclude       # All excluded years
      yrs <- yrs[!yrs %in% vf_excluded()] # Not ones excluded in last run (point doesn't exist)

      if(length(yrs) == 0) yrs <- NULL
      if(!identical(yrs, input$vf_plot_selected)) { # Don't change if no change to make
        if(is.null(yrs)) yrs <- ""
        session$sendCustomMessage(type = 'vf_plot_set', message = yrs)
      }
  }) %>%
    bindEvent(input$vf_years_exclude, ignoreNULL = FALSE, ignoreInit = TRUE)






  ## Table - Plot data -----------------------
  output$vf_table_plot <- DT::renderDT({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$vf_compute,
             "Choose your settings and click 'Compute Analysis'"))

    vf_freqs()[["Freq_Plot_Data"]] %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })

  ## Table - Fitted Quantiles -----------------------
  output$vf_table_fit <- DT::renderDT({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$vf_compute,
             "Choose your settings and click 'Compute Analysis'"))

    vf_freqs()[["Freq_Fitted_Quantiles"]] %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })

  ## Fit checks --------------------
  output$vf_fit_stats <- renderPrint({
    req(vf_freqs(), input$vf_day)
    vf_freqs()[["Freq_Fitting"]][[input$vf_day]]
  })

  output$vf_fit_plot <- renderPlot({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$vf_compute,
             "Choose your settings and click 'Compute Analysis'"))

    req(input$vf_day)
    vf_freqs()[["Freq_Fitting"]][[input$vf_day]] %>%
      fasstrshiny:::gg_fitdistr(title = input$vf_day)
  })



  ## R Code -----------------
  output$vf_code <- renderText({
    fasstrshiny:::code_format(code, id = "vf")
  })



  # Volume Frequency - HYDAT Peak -------------------------------------------

  ## Frequencies -----------------------
  hp_freqs <- reactive({
    req(data_raw(), input$hp_use_max)

    validate(need(
      isTruthy(data_raw()$STATION_NUMBER) &
        length(unique(data_raw()$STATION_NUMBER)) == 1,
      paste0("This analysis is only available for HYDAT data with a ",
             "valid STATION_NUMBER")))

    validate(need(all(!is.na(fasstrshiny:::text_to_num(input$hp_prob_scale))),
                  "Probabilies to plot must be a comma separated list of numbers"))

    data_flow <- data_raw()

    # Define parameters
    p <- c(
      glue("station_number = '{unique(data_flow$STATION_NUMBER)}'"),
      glue("use_max = {input$hp_use_max}"),
      glue("use_log = {input$hp_log}"),
      glue("prob_plot_position = '{input$hp_prob_plot}'"),
      glue("prob_scale_points = c({input$hp_prob_scale})"),
      glue("fit_distr = '{input$hp_fit_distr}'"),
      glue("fit_quantiles = c({glue_collapse(input$hp_fit_quantiles, sep = ', ')})"),
      glue("plot_curve = {input$vf_plot_curve}")) %>%
      glue_collapse(sep = ", ")

    r <- create_fun(
      "compute_hydat_peak_frequencies", id = "hp", input = input,
      params_ignore = c("roll_days", "roll_align", "water_year", "months"),
      extra = p)

    code$hp_data <- r

    eval(parse(text = r))
  }) %>%
    bindEvent(input$hp_compute)

  ## Plot --------------------
  output$hp_plot <- renderGirafe({

    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$hp_compute,
             "Choose your settings and click 'Compute Analysis'"))

    req(hp_freqs())

    # Add interactivity
    g <- hp_freqs()[["Freq_Plot"]] +
      geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n",
                         "Probabily: ", round(prob, 4), "\n",
                         "Discharge: ", round(Value, 4), "\n",
                         "Return Period: ", round(Return_P)),
        data_id = Year), size = 4)

    girafe(ggobj = g,
           options = list(opts_selection(type = "none"),
                          opts_toolbar(position = "topleft")))
  })


  ## Table -----------------------
  output$hp_table <- DT::renderDT({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$hp_compute,
             "Choose your settings and click 'Compute Analysis'"))

    hp_freqs()[["Freq_Fitted_Quantiles"]] %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Brtip'))
  })

  ## Fit checks --------------------
  output$hp_fit_stats <- renderPrint({
    req(hp_freqs())
    hp_freqs()[["Freq_Fitting"]][[1]]
  })

  output$hp_fit_plot <- renderPlot({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$hp_compute,
             "Choose your settings and click 'Compute Analysis'"))
    hp_freqs()[["Freq_Fitting"]][[11]] %>%
      fasstrshiny:::gg_fitdistr(title = "")
  })



  ## R Code -----------------
  output$hp_code <- renderText({
    fasstrshiny:::code_format(code, id = "hp")
  })


  # Outputs to NOT suspend when hidden -------------

  # Ensure that ui elements are not suspended when hidden
  map(str_subset(names(outputOptions(output)), "^ui_"),
      ~outputOptions(output, ., suspendWhenHidden = FALSE))
}





