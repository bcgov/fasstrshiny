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

  meta <- reactiveValues(station_name = "",
                         basin_area = NA_real_)


  # UI elements ---------------------------------------

  ## Data ----------------------
  output$ui_data_water_year <- renderUI({
    validate(need(input$data_load,
                  "You'll need to first load some data"))
    radioGroupButtons(
      "data_water_year",
      label = "Water year start",
      choices = setNames(1:12, month.abb),
      selected = 1, size = "sm", width = "100%")
  })

  output$ui_data_years_range <- renderUI({
    req(data_raw())
    sliderInput("data_years_range",
                label = "Start and end years",
                min = min(data_raw()$WaterYear),
                max = max(data_raw()$WaterYear),
                value = c(min(data_raw()$WaterYear),
                          max(data_raw()$WaterYear)),
                dragRange = TRUE, sep = "")
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

    selectizeInput("data_years_exclude",
                   label = "Years to exclude",
                   choices = seq(from = input$data_years_range[1],
                                 to = input$data_years_range[2], by = 1),
                   selected = s,
                   multiple = TRUE)
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
      bsTooltip("data_months", tips$months))
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


  # Add plot options as Gear in corner
  output$ui_data_plot_options <- renderUI({
    req(data_raw())
    select_plot_options(data = data_raw(), id = "data", input,
                        include = c("plot_log", "daterange"))
  })

  ## SS - General -------------------------------------------------------------
  output$ui_sum <- renderUI({
    build_ui(id = "sum", input,
             include = c("discharge", "complete"))
  })

  output$ui_sum_monthly_plot <- renderUI({
    req(input$sum_type == "Monthly")

    p <- c("Mean", "Median", "Maximum", "Minimum")
    if(!is.null(input$sum_percentiles)) p <- c(p, glue("P{input$sum_percentiles}"))

    selectizeInput("sum_monthly_plot", label = "Statistic to plot",
                   choices = p, selected = 1)

  })

  # Separate so when it recomputes, not all the other inputs also change
  output$ui_sum_miss_allowed <- renderUI({
    select_miss_allowed("sum", input)
  })

  # Plot options
  output$ui_sum_plot_options <- renderUI({

    select_plot_options(id = "sum", input,
                        include = c("percentiles",
                                    "plot_log", "add_year",
                                    "add_dates", "add_mad"))
    # Add inner/outer percentiles?
  })


  # Enable/disable based on type
  observe({
    # add year
    if(input$sum_type %in% c("Long-term", "Daily")) {
      enable("sum_add_year")
    } else {
      disable("sum_add_year")
    }

    # add dates
    if(input$sum_type == "Daily") {
      enable("sum_add_dates")
    } else {
      disable("sum_add_dates")
    }

    # custom months
    if(input$sum_type == "Long-term") {
      enable("sum_custom_months")
      enable("sum_custom_months_label")
    } else {
      disable("sum_custom_months")
      disable("sum_custom_months_label")
    }

  }) %>%
    bindEvent(input$sum_type)

  # Table options
  output$ui_sum_table_options <- renderUI({
    select_table_options(id = "sum", input)
  })

  ## SS - Flow ----------------------------------------------------------
  output$ui_sumfl <- renderUI({
    build_ui(id = "sumfl", input,
             include = c("discharge", "complete", "missing", "custom_months"))
  })

  # Plot options
  output$ui_sumfl_plot_options <- renderUI({
    select_plot_options(data = data_raw(), id = "sumfl", input,
                        include = c("plot_log"))
  })

  ## SS - Annual Means ----------------------------------------------------------
  output$ui_sumam <- renderUI({
    build_ui(id = "sumam", input, include = "missing")
  })


  ## Cumulative ----------------------------------------------------------

  # Plot options
  output$ui_cum_seasons <- renderUI({
    # Enable/disable season
    req(input$cum_type)

    # Get previous value
    if(!is.null(input$cum_seasons)) value <- input$cum_seasons else value <- TRUE

    # If not annual, disable
    if(input$cum_type != "Annual") {
      m <- materialSwitch("cum_seasons",
                          label = tags$span("Include seasons",
                                            id = "cum_seasons_tip"),
                          value = value, status = "default") %>%
        disabled()
    } else {
      m <- materialSwitch("cum_seasons",
                          label = tags$span("Include seasons",
                                            id = "cum_seasons_tip"),
                          value = value, status = "success")
    }

    tagList(m, bsTooltip("cum_seasons_tip", tips$seasons))
  }) %>%
    bindEvent(input$cum_type)

  output$ui_cum_plot_options <- renderUI({
    select_plot_options(data = data_raw(), id = "cum", input,
                        include = c("plot_log", "add_year"))
  })

  # Table options
  output$ui_cum_table_options <- renderUI({
    select_table_options(data = data_raw(), id = "cum", input,
                         include = "percentiles")
  })


  # Enable/disable based on type
  observe({
    # add year
    if(input$cum_type %in% c("Monthly", "Daily")) {
      enable("cum_add_year")
    } else {
      disable("cum_add_year")
    }
  }) %>%
  bindEvent(input$cum_type)



  ## AH - Flow timing --------------------------------------------------------
  # None (months set globally, not per tab)

  ## AH - Low flows --------------------------------------------------------
  output$ui_ahlf <- renderUI({
    build_ui(id = "ahlf", input,
             include = c("discharge", "allowed"))
  })

  ## AH - Peak flows --------------------------------------------------------
  output$ui_ahp <- renderUI({
    build_ui(id = "ahp", input,
             include = c("discharge", "allowed"))
  })

  ## AH - Days outside normal -----------------------------------------------
  # None


  ## Annual Trends ------------------------------------------------

  # Excluded years, takes defaults from input$data_years_exclude,
  # but allowed to modify here
  output$ui_at_exclude <- renderUI({
    req(input$data_years_range)
    selectizeInput("at_years_exclude",
                   label = "Years to exclude",
                   choices = seq(from = input$data_years_range[1],
                                 to = input$data_years_range[2], by = 1),
                   selected = input$data_years_exclude,
                   multiple = TRUE)
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
                  value = input$opts_allowed, step = 5, min = 0, max = 100),
      sliderInput("at_allowed_monthly",
                  label = "Monthly - Allowed missing (%)",
                  value = input$opts_allowed, step = 5, min = 0, max = 100),
      bsTooltip("at_allowed_annual", tips$allowed),
      bsTooltip("at_allowed_monthly", tips$allowed)
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

  output$ui_vf <- renderUI({
    build_ui(id = "vf", input, include = c("discharge", "allowed"))
  })

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
    req(input$data_station_num)

    wy <- 1
    if(!is.null(input$data_water_year)) wy <- as.numeric(input$data_water_year)


    if (input$data_source == "HYDAT") {
      m <- filter(stations, .data$station_number == input$data_station_num)
      meta$station_name <- m$station_name
      meta$basin_area <- as.numeric(m$drainage_area_gross)

      d <- glue(
        "flow_data <- fill_missing_dates(",
        "        station_number = '{input$data_station_num}') %>%",
        "  add_date_variables(water_year_start = {wy}) %>%",
        "  add_daily_volume() %>%",
        "  add_daily_yield()")

    } else {
      inFile <- input$data_file
      if (is.null(inFile)) return(NULL)

      meta$station_name <- input$data_station_name
      meta$basin_area <- as.numeric(input$data_basin_area)

      d <- glue("flow_data <- read.csv({inFile$datapath}) %>%",
                "  fill_missing_dates() %>%",
                "  add_date_variables(water_year_start = {wy})")

    }

    # Save unevaluated code for code tab
    code$data_raw <- d

    eval(parse(text = d)) # Evaluate and create flow_data
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
        input$data_water_year)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_flow_data", "flow_data", id = "data", input,
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

  # Data - Screening ---------------
  ## Data --------------
  screen_raw <- reactive({
    req(data_raw())

    flow_data <- data_raw()
    d <- glue("
      screen_data <- screen_flow_data(
        data = flow_data,
        start_year = {input$data_years_range[1]},
        end_year = {input$data_years_range[2]},
        water_year_start = {as.numeric(input$data_water_year)})
        ")

    code$screen_data <- d
    eval(parse(text = d))
  })

  ## Summary plot ------------------
  output$screen_plot1 <- renderGirafe({
    check_data(input)
    req(screen_raw())

    xlab <- if_else(input$data_water_year != 1, "Water Year", "Year")
    title <- paste0("Annual Daily ", input$screen_summary, " - ", meta$station_name)
    screen_data <- screen_raw()

    g <- glue(
      "ggplot(data = screen_data,
              aes(x = Year, y = {input$screen_summary})) +
        theme_bw() +
        theme(axis.title = element_text(size = 15),
              plot.title = element_text(size = 15, hjust = 0.5),
              axis.text = element_text(size = 13)) +
        geom_line(colour = 'dodgerblue4') +
        geom_point(colour = 'firebrick3', size = 2) +
        labs(x = '{xlab}', y = 'Discharge (cms)', title = '{title}')")

    code$screen_plot <- g

    g <- parse(text = g) %>%
      eval() +
      geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n",
                         input$screen_summary, ": ",
                         round(.data[[input$screen_summary]], 4)),
        data_id = Year),
        colour = 'firebrick3', size = 3)

    girafe(ggobj = g, width_svg = 13, height_svg = 4.5)
  })


  ## Missing Data Plot ---------------------------
  output$screen_plot2 <- renderGirafe({
    check_data(input)
    req(data_raw())

    flow_data <- data_raw()
    g <- create_fun(
      "plot_missing_dates", data = "flow_data",
      id = "screen", input,
      params_ignore = "months",
      extra = glue("months = c({glue_collapse(input$screen_months, sep = ', ')})"),
      end = "[[1]]")

    code$screen_miss <- g

    g <- parse(text = g) %>%
      eval()

    # Replace layers with interactive
    g$layers[[1]] <- geom_bar_interactive(
      aes(tooltip = paste0("Year: ", Year, "\nMissing: ", Value),
          data_id = paste0(Year, "-", Month)), colour = "cornflowerblue",
      fill = "cornflowerblue", stat = "identity")

    girafe(ggobj = g, width_svg = 14, height_svg = 6,
           options = list(opts_selection(type = "none")))
  })

  ## Summary table ------------------
  output$screen_table <- DT::renderDT({
    check_data(input)
    screen_raw() %>%
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
  output$screen_code <- renderText({
    fasstrshiny:::code_format(code, id = "screen")
  })



  # Summary Stats - General ---------------------------------------

  ## Plot --------------------
  output$sum_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$sum_type,
        !is.null(input$sum_plot_log), !is.null(input$sum_add_year))

    flow_data <- data_raw()

    e <- NULL
    if(input$sum_type %in% c("Long-term", "Daily") & input$sum_add_year != "") {
      e <- glue("add_year = {input$sum_add_year}")
    }

    if(input$sum_type %in% c("Long-term", "Daily")) {
      p <- c("complete", "missing")
    } else {
      p <- c("allowed", "percentiles")
    }

    p <- c(p, "plot_log")

    end <- "[[1]]"

    if(input$sum_type == "Monthly" & !is.null(input$sum_monthly_plot)) {
      end <- glue("[['{input$sum_monthly_plot}_Monthly_Statistics']]")
    }

    g <- switch(input$sum_type,
                "Long-term" = "plot_longterm_daily_stats",
                "Annual" = "plot_annual_stats",
                "Monthly" = "plot_monthly_stats",
                "Daily" = "plot_daily_stats") %>%
      create_fun("flow_data", id = "sum", input,
                 params = p,
                 extra = e,
                 end = end)

    code$sum_plot <- g

    g <- eval(parse(text = g))


    # Add interactivity
    if(input$sum_type == "Long-term") {
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("LT_Mean", "LT_Med")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Year_mean"] <- input$sum_add_year

      # Add interactive vline
      g <- g + fasstrshiny:::create_vline_interactive(
        data = g$data, stats = stats, size = 20)

    } else if(input$sum_type == "Annual") {
      # Replace point layers with interactive ones
      which_pt <- map_lgl(g$layers, ~any(class(.$geom) %in% "GeomPoint"))
      g$layers[which_pt][[1]] <- geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n", Statistic, ": ", round(Value, 4)),
        data_id = Year), size = 4)

    } else if(input$sum_type == "Monthly") {
      req(input$sum_monthly_plot)
      g <- g +
        geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n", "Month: ", Month, "\n",
                         Stat2, ": ", round(Value, 4)),
        data_id = Year))
    } else if(input$sum_type == "Daily") {
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("DayofYear", "AnalysisDate")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Date"] <- "Day"
      names(stats)[stats == "RollingValue"] <- input$sum_add_year

      # Add Interactive vline
      g <- g + fasstrshiny:::create_vline_interactive(data = g$data, stats)
    }

    # Add dates
    if(input$sum_type == "Daily" & !is.null(input$sum_add_dates)){
      dts <- data.frame(
        Date = fasstrshiny:::get_date(
          input$sum_add_dates,
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
    if(!is.null(input$sum_add_mad) && input$sum_add_mad &&
       input$sum_type != "Monthly") {

      mad <- sum_mad() %>%
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
  sum_mad <- reactive({
    req(input$sum_discharge, input$sum_mad)

    flow_data <- data_raw()

    t <- create_fun(
      fun = "calc_longterm_mean",
      data = "flow_data", id = "sum", input,
      params = "complete",
      extra = glue("percent_MAD = c({glue_collapse(input$sum_mad, sep = ',')})"))

    code$sum_mad <- t

    parse(text = t) %>%
      eval()
  })

  output$sum_mad <- render_gt({
    check_data(input)
     sum_mad() %>%
      gt() %>%
      fmt_number(columns = where(is.numeric), decimals = 4)
  })

  ## Table -----------------------
  output$sum_table <- DT::renderDT({
    check_data(input)
    req(input$sum_type, input$sum_discharge)

    flow_data <- data_raw()

    p <- "percentiles"

    p <- switch(input$sum_type,
                "Long-term" = c("complete", "missing", "custom_months",
                                "custom_months_label"),
                "Annual" = "allowed",
                "Monthly" = "allowed",
                "Daily" = "complete", "missing")

    p <- c(p, "percentiles")

    t <- switch(input$sum_type,
                "Long-term" = "calc_longterm_daily_stats",
                "Annual" = "calc_annual_stats",
                "Monthly" = "calc_monthly_stats",
                "Daily" = "calc_daily_stats") %>%
      create_fun("flow_data", id = "sum", input,
                 params = p)

    code$sum_table <- t

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
  output$sum_code <- renderText({
    fasstrshiny:::code_format(code, id = "sum")
  })


  # Summary Stats - Flow ---------------------------------------

  ## Flow Percentile -----------------------
  output$sumfl_perc <- renderText({
    req(input$sumfl_discharge, input$sumfl_flow)

    flow_data <- data_raw()

    # Flow
    t <- create_fun(fun = "calc_flow_percentile",
                    data = "flow_data", id = "sumfl", input,
                    params = "complete",
                    extra = glue("flow_value = {input$sumfl_flow}"))

    code$sumfl_flow <- t

    parse(text = t) %>%
      eval() %>%
      pull(Percentile) %>%
      round(4)
  })

  ## Plot --------------------
  output$sumfl_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), !is.null(input$sumfl_plot_log))

    flow_data <- data_raw()

    # missing arguments
    # - include_longterm

    g <- create_fun(
      fun = "plot_flow_duration", data = "flow_data", id = "sumfl", input,
      params = c("custom_months", "custom_months_label", "complete",
                 "missing", "plot_log"),
      end = "[[1]]")

    code$sumfl_plot <- g

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
  output$sumfl_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$sumfl_discharge)

    flow_data <- data_raw()

    t <- create_fun(fun = "calc_longterm_daily_stats",
                    data = "flow_data", id = "sumfl", input,
                    params = c("custom_months", "custom_months_label",
                               "missing"),
                    extra = "percentiles = 1:99",
                    end = "%>% select(-Mean, -Median, -Minimum, -Maximum)")

    code$sumfl_table <- t

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
  output$sumfl_code <- renderText({
    fasstrshiny:::code_format(code, id = "sumfl")
  })



  # Summary Stats - Annual Means ---------------------------------------


  ## Plot --------------------
  output$sumam_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$sumam_missing)

    flow_data <- data_raw()

    g <- create_fun(
      fun = "plot_annual_means", data = "flow_data", id = "sumam", input,
      params = c("missing"),
      end = "[[1]]")

    code$sumam_plot <- g

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
  output$sumam_code <- renderText({
    fasstrshiny:::code_format(code, id = "sumam")
  })





  # Cumulative ---------------------------------------
  ## Plot --------------------
  output$cum_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$cum_type, !is.null(input$cum_add_year),
        !is.null(input$cum_seasons))

    flow_data <- data_raw()

    e <- glue("use_yield = {input$cum_discharge}")

    if(input$cum_type == "Annual") {
      p <- NULL
      e <- glue("{e}, include_seasons = {input$cum_seasons}")
      end <- ""
    } else {
      p <- "plot_log"
      end <- "[[1]]"
      if(input$cum_add_year != "") {
        e <- glue("{e}, add_year = {input$cum_add_year}")
      }
    }

    g <- switch(input$cum_type,
                "Annual"  = "plot_annual_cumulative_stats",
                "Monthly" = "plot_monthly_cumulative_stats",
                "Daily"   = "plot_daily_cumulative_stats") %>%
      create_fun("flow_data",
                 id = "cum", input, params = p,
                 params_ignore = c("discharge", "roll_days", "roll_align"),
                 extra = e,
                 end = end)

    code$cum_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))

    if(input$cum_type == "Annual") {

      # Add individual geoms to each plot (annual has more than one)
      for(i in seq_along(g)) {
        g[[i]] <- g[[i]] + geom_point_interactive(
          aes(tooltip = paste0("Year: ", Year, "\n",
                               Statistic, ": ", round(Value, 4)),
              data_id = Year), size = 3)
      }

      g <- g %>%
        wrap_plots(nrow = 2, byrow = FALSE, design = "AC
                                                      BC")
    } else {
      stats <- names(g$data) # Get stats from plot data
      stats <- stats[!stats %in% c("WaterYear", "AnalysisDate", "DayofYear")] # Omit these

      # For tooltips labels...
      names(stats)[stats == "Monthly_Total"] <- input$cum_add_year
      names(stats)[stats == "Cumul_Flow"] <- input$cum_add_year

      # Add interactive vline
      g <- g + fasstrshiny:::create_vline_interactive(
        data = g$data, stats = stats,
        size = if_else(input$cum_type == "Monthly", 20, 1))

    }

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

    flow_data <- data_raw()

    e <- glue("use_yield = {input$cum_discharge}")
    if(input$cum_type == "Annual") {
      e <- glue("{e}, include_seasons = {input$cum_seasons}")
      p <- NULL
    } else {
      p <- "percentiles"
    }

    t <- switch(input$cum_type,
                "Annual"  = "calc_annual_cumulative_stats",
                "Monthly" = "calc_monthly_cumulative_stats",
                "Daily"   = "calc_daily_cumulative_stats") %>%
      create_fun("flow_data",
                 id = "cum", input, params = p,
                 params_ignore = c("discharge", "roll_days", "roll_align"),
                 extra = e)

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


  # AH - Flow timing ---------------------------------------
  ## Plot --------------------
  output$ahft_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$ahft_percent)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_annual_flow_timing", data = "flow_data",
      id = "ahft", input,
      params_ignore = c("roll_days", "roll_align"),
      extra = glue("percent_total = ",
                   "c({glue_collapse(input$ahft_percent, sep = ',')})"),
      end = "[[1]]")

    code$ahft_plot <- g

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
  output$ahft_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$ahft_percent)

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_flow_timing", data = "flow_data",
      id = "ahft", input,
      params_ignore = c("roll_days", "roll_align"),
      extra = glue("percent_total = ",
                   "c({glue_collapse(input$ahft_percent, sep = ',')})"))

    code$ahft_table <- t

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
  output$ahft_code <- renderText({
    fasstrshiny:::code_format(code, id = "ahft")
  })


  # AH - Low Flows ---------------------------------------
  ## Plot --------------------
  output$ahlf_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$ahlf_discharge)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_annual_lowflows", data = "flow_data",
      id = "ahlf", input,
      params = c("discharge", "allowed"),
      params_ignore = c("roll_days", "roll_align"),
      extra = glue(
        "roll_days = c({glue_collapse(input$ahlf_roll_days, sep = ',')}), ",
        "roll_align = '{input$ahlf_roll_align}'"))

    code$ahlf_plot <- g

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
  output$ahlf_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$ahlf_discharge)

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_lowflows", data = "flow_data",
      id = "ahlf", input,
      params = c("discharge", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$ahlf_roll_days, sep = ',')})"))

    code$ahlf_table <- t

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
  output$ahlf_code <- renderText({
    fasstrshiny:::code_format(code, id = "ahlf")
  })

  # AH - Peaks ---------------------------------------

  ## Table -----------------------
  output$ahp_table <- DT::renderDT({
    check_data(input)
    req(data_raw(), input$ahp_discharge)

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_peaks", data = "flow_data",
      id = "ahp", input,
      params = c("discharge", "missing", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$ahp_roll_days, sep = ',')})"))

    code$ahp_table <- t

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
  output$ahp_code <- renderText({
    fasstrshiny:::code_format(code, id = "ahp")
  })

  # AH - Days outside normal ---------------------------------------
  ## Plot --------------------
  output$ahon_plot <- renderGirafe({
    check_data(input)
    req(data_raw(), input$ahon_normal)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_annual_outside_normal", data = "flow_data",
      id = "ahon", input,
      extra = glue("normal_percentiles = ",
                   "c({glue_collapse(input$ahon_normal, sep = ',')})"),
      end = "[[1]]")

    code$ahon_plot <- g

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
  output$ahon_table <- DT::renderDT({
    req(data_raw(), input$ahon_normal)

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_outside_normal", data = "flow_data",
      id = "ahon", input,
      params = "discharge",
      extra = glue("normal_percentiles = ",
                   "c({glue_collapse(input$ahon_normal, sep = ',')})"))

    code$ahon_table <- t

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
  output$ahon_code <- renderText({
    fasstrshiny:::code_format(code, id = "ahon")
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

    flow_data <- data_raw()

    # ingore_missing / allowed missing
    #basin area?

    # Define parameters
    p <- c(
      glue("exclude_years = c({glue_collapse(input$at_years_exclude, sep = ', ')})"),
      glue("zyp_method = '{input$at_zyp}'"),
      glue("annual_percentiles = c({glue_collapse(input$at_annual_percentiles, sep = ', ')})"),
      glue("monthly_percentiles = c({glue_collapse(input$at_monthly_percentiles, sep = ', ')})"),
      glue("stats_days = {input$opts_roll_days}"),
      glue("stats_align = '{input$opts_roll_align}'"),
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
      data = "flow_data", id = "at", input,
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
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$at_compute,
             "Choose your settings and click 'Compute Trends'"))

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

    flow_data <- data_raw()

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
      data = "flow_data", id = "vf", input,
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

    flow_data <- data_raw()

    # Define parameters
    p <- c(
      glue("station_number = '{unique(flow_data$STATION_NUMBER)}'"),
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


}





