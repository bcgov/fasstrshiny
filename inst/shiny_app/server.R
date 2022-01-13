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

  # Disable allowed missing depend on on type and whether ignoring missing
  toggle_allowed(id = "sum", input)

  ## Data ----------------------

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

  # Year selection/slider UI
  output$ui_data_water_year <- renderUI({
    req(data_raw())
    tagList(
      h4("Filter Dates"),
      selectInput("data_water_year",
                  label = "Water year",
                  choices = list("Jan-Dec" = 1, "Feb-Jan" = 2,
                                 "Mar-Feb" = 3, "Apr-Mar" = 4,
                                 "May-Apr" = 5, "Jun-May" = 6,
                                 "Jul-Jun" = 7, "Aug-Jul" = 8,
                                 "Sep-Aug" = 9, "Oct-Sep" = 10,
                                 "Nov-Oct" = 11, "Dec-Nov" = 12),
                  selected = 1))
  })

  # Years range
  output$ui_data_years_range <- renderUI({
    req(data_raw(), input$data_water_year)
    sliderInput("data_years_range",
                label = "Start and end years",
                min = min(data_raw()$WaterYear),
                max = max(data_raw()$WaterYear),
                value = c(min(data_raw()$WaterYear), max(data_raw()$WaterYear)),
                dragRange = TRUE, sep = "")
  })

  # Exclude years selection
  output$ui_data_years_exclude <- renderUI({
    req(data_raw(), input$data_years_range)
    selectizeInput("data_years_exclude",
                   label = "Years to exclude",
                   choices = seq(from = input$data_years_range[1],
                                 to = input$data_years_range[2], by = 1),
                   selected = NULL,
                   multiple = TRUE)
  })

  # Add plot options as Gear in corner
  output$ui_data_plot_options <- renderUI({
    req(data_raw())
    select_plot_options(data = data_raw(), id = "data_plot", input)
  })

  ## Summary -------------------
  output$ui_sum <- renderUI({
    build_ui(id = "sum", input,
             include = c("discharge", "missing", "allowed", "rolling",
                         "months", "percentiles"))
  })

  # Add plot options as Gear in corner
  output$ui_sum_plot_options <- renderUI({
    p <- select(data_raw(), -any_of(c("STATION_NUMBER", "Month", "Year"))) %>%
      names()

    select_plot_options(data = data_raw(), id = "sum_plot", input,
                        include = c("log", "parameters"),
                        params = p)
  })

  ## Summary - Flow ------------
  output$ui_sumfl <- renderUI({
    build_ui(id = "sumfl", input,
             include = c("discharge", "missing", "allowed", "months"))
  })

  # Plot options
  output$ui_sumfl_plot_options <- renderUI({
    select_plot_options(data = data_raw(), id = "sumfl_plot", input,
                        include = c("log"))
  })

  ## Summary - Single ------------
  output$ui_sumsi <- renderUI({
    build_ui(id = "sumsi", input,
             include = c("discharge", "months", "percentiles"))
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
                               dom = 'Bfrtip'))
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

    updateTabsetPanel(session, inputId = "data_tabs", selected = "data_plot")

    # Save unevaluated code for code tab
    code$data_raw <- d

    eval(parse(text = d)) # Evaluate and create flow_data
  }) %>%
    bindEvent(input$data_load)

  ## Plot ----------------
  output$data_plot <- renderPlotly({
    req(data_raw(),
        !is.null(input$data_plot_log),
        input$data_plot_daterange,
        input$data_years_range,
        input$data_water_year)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_flow_data", "flow_data", id = "data", input,
      params = c("water_year", "years_range", "exclude_years",
                 "plot_log", "plot_daterange"),
      end = "[[1]] + scale_color_manual(values = 'dodgerblue4')")

    code$data_plot <- g

    parse(text = g) %>%
      eval() %>%
      ggplotly()
  })

  ## Table ----------------
  output$data_table <- renderDT({
    data_raw() %>%
      rename("StationNumber" = "STATION_NUMBER") %>%
      select(-"Month") %>%
      mutate(Value = round(Value, 4)) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Bfrtip'))
  })

  ## R Code ----------------
  output$data_code <- renderText({
    code_format(code, id = "data")
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
  output$screen_plot1 <- renderPlotly({
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

    parse(text = g) %>%
      eval() %>%
      ggplotly()
  })


  ## Missing Data Plot ---------------------------
  output$screen_plot2 <- renderPlotly({
    req(data_raw())

    flow_data <- data_raw()
    g <- create_fun("plot_missing_dates", "flow_data",
                    id = "screen", input,
                    params = c("data" = "data_years_range",
                               "data" = "water_year",
                               "months"), end = "[[1]]")

    code$screen_miss <- g

    parse(text = g) %>%
      eval() %>%
      ggplotly()
  })

  ## Summary table ------------------
  output$screen_table <- DT::renderDT({
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
                               dom = 'Bfrtip'))
  })

  ## R Code -----------------
  output$screen_code <- renderText({
    code_format(code, id = "screen")
  })



  # Summary Statistics - General ---------------------------------------

  ## Plot --------------------
  output$sum_plot <- renderPlot({
    req(data_raw(), input$sum_plot_params)

    flow_data <- data_raw()

    g <- switch(input$sum_type,
                "Long-term" = "plot_longterm_daily_stats",
                "Annual" = "plot_annual_stats",
                "Monthly" = "plot_monthly_stats",
                "Daily" = "plot_daily_stats") %>%
      create_fun("flow_data", id = "sum", input,
                 params = c("discharge", "roll_days", "roll_align", "months",
                            "data" = "water_year",
                            "data" = "years_range",
                            "data" = "years_exclude",
                            if_else(input$sum_type %in% c("Long-term", "Daily"),
                                    "missing", "allowed"),
                            "plot_log"),
                 end = "[[1]]")

    code$sum_plot <- g

    eval(parse(text = g))
  })

  ## Table -----------------------
  output$sum_table <- DT::renderDT({
    req(input$sum_type, input$sum_discharge)

    flow_data <- data_raw()

    t <- switch(input$sum_type,
                "Long-term" = "calc_longterm_daily_stats",
                "Annual" = "calc_annual_stats",
                "Monthly" = "calc_monthly_stats",
                "Daily" = "calc_daily_stats") %>%
      create_fun("flow_data", id = "sum", input,
                 params = c("discharge", "percentiles",
                            "roll_days", "roll_align",
                            "data" = "water_year",
                            "data" = "years_range",
                            "data" = "years_exclude",
                            "months",
                            if_else(input$sum_type %in% c("Long-term", "Daily"),
                                    "missing", "allowed")))

    code$sum_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Bfrtip'))
  })


  ## R Code -----------------
  output$sum_code <- renderText({
    code_format(code, id = "sum")
  })


  # Summary Statistics - Flow ---------------------------------------
  ## Plot --------------------
  output$sumfl_plot <- renderPlot({
    req(data_raw(), !is.null(input$sumfl_plot_log))

    flow_data <- data_raw()

    # missing arguments
    # - include_longterm
    # - custom months

    g <- create_fun(
      fun = "plot_flow_duration", data = "flow_data", id = "sumfl", input,
      params = c("discharge",
                 "roll_days", "roll_align",
                 "data" = "water_year",
                 "data" = "years_range",
                 "data" = "years_exclude",
                 "months", "missing", "plot_log"),
      end = "[[1]]")

    code$sumfl_plot <- g

    eval(parse(text = g))
  })


  ## Table -----------------------
  output$sumfl_table <- DT::renderDT({
    req(data_raw(), input$sumfl_discharge)

    flow_data <- data_raw()

    t <- create_fun(fun = "calc_longterm_daily_stats",
                    data = "flow_data", id = "sumfl", input,
                    params = c("discharge", "roll_days", "roll_align",
                               "data" = "water_year",
                               "data" = "years_range",
                               "data" = "years_exclude",
                               "months", "missing"),
                    extra = "percentiles = 1:99",
                    end = "%>% select(-Mean, -Median, -Minimum, -Maximum)")

    code$sumfl_table <- t

    parse(text = t) %>%
      eval() %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(rownames = FALSE,
                filter = 'top',
                extensions = c("Scroller"),
                options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                               deferRender = TRUE, dom = 'Bfrtip'))
  })


  ## R Code -----------------
  output$sumfl_code <- renderText({
    code_format(code, id = "sumfl")
  })

  # Summary Statistics - Single ---------------------------------------

  ## MAD -----------------------
  output$sumsi_mad <- render_gt({
    req(input$sumsi_discharge)
    validate(need(
      is.null(input$sumsi_mad) ||
        input$sumsi_mad == "" ||
        !any(is.na(as.numeric(str_split(input$sumsi_mad,
                             pattern = ",", simplify = TRUE)))),
      "MAD percentiles must be comma-separated numbers (e.g., 5, 10, 50)"))

    flow_data <- data_raw()

    # Args missing: complete_years
    t <- create_fun(fun = "calc_longterm_mean",
                    data = "flow_data", id = "sumsi", input,
                    params = c("discharge", "roll_days", "roll_align",
                               "data" = "water_year",
                               "data" = "years_range",
                               "data" = "years_exclude",
                               "months", "mad"))

    code$sumsi_mad <- t

    parse(text = t) %>%
      eval() %>%
      gt() %>%
      fmt_number(columns = where(is.numeric), decimals = 4)
  })

  ## Perc -----------------------
  output$sumsi_perc <- render_gt({
    req(input$sumsi_discharge)

  # Percentiles
  t <- create_fun(fun = "calc_longterm_percentile",
                  data = "flow_data", id = "sumsi", input,
                  params = c("discharge", "roll_days", "roll_align",
                             "data" = "water_year",
                             "data" = "years_range",
                             "data" = "years_exclude",
                             "months", "percentiles"))

  code$sumsi_perc <- t

  parse(text = t) %>%
    eval() %>%
    gt() %>%
    fmt_number(decimals = 4)
  })

  # ADD:
  # calc_flow_percentile()

  ## R Code -----------------
  output$sumfl_code <- renderText({
    code_format(code, id = "sumfl")
  })




# Older code -------------------------------


  # Flow Duration and Percentiles

  ptile_data <- reactive({

    ptile_data <- data_raw()

    calc_longterm_daily_stats(data = ptile_data,
                              percentiles = seq(from = 1,  to = 99, by = 1),
                              roll_days = input$ptile_roll_days,
                              roll_align = input$ptile_roll_align,
                              water_year_start = as.numeric(input$data_water_year),
                              start_year = input$data_years_range[1],
                              end_year = input$data_years_range[2],
                              exclude_years = as.numeric(input$data_years_exclude),
                              custom_months = as.numeric(input$ptile_months_cust),
                              custom_months_label = input$ptile_months_cust_label,
                              ignore_missing = input$ptile_ign_missing_box) %>%
      select(-Mean, -Median, -Minimum, -Maximum)
  })

  output$ptile_table <- DT::renderDataTable(
    ptile_data() %>% select(-dplyr::contains("STATION_NUMBER")) %>%
      mutate_if(is.numeric, funs(round(., 4))),
    rownames = FALSE,
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip')
  )
  output$download_ptile_table <- downloadHandler(
    filename = function() {paste0(meta$station_name," - Long-term Percentiles.csv")},
    content = function(file) {
      write.csv(ptile_data(), file, row.names = FALSE)
    })

  ptile_plot_data <- reactive({
    ptile_data() %>% gather(Parameter, Value, 3:ncol(ptile_data()))
  })


  ptile_months_plotting <- reactive({

    if (!is.null(input$ptile_months_cust) & !is.null(input$ptile_months_cust_label)) {
      list("Jan" = 1, "Feb" = 2,
           "Mar" = 3, "Apr" = 4,
           "May" = 5, "Jun" = 6,
           "Jul" = 7, "Aug" = 8,
           "Sep" = 9, "Oct" = 10,
           "Nov" = 11, "Dec" = 12,
           "Long-term" = 13,
           "Custom Months" = 14)
    } else {
      list("Jan" = 1, "Feb" = 2,
           "Mar" = 3, "Apr" = 4,
           "May" = 5, "Jun" = 6,
           "Jul" = 7, "Aug" = 8,
           "Sep" = 9, "Oct" = 10,
           "Nov" = 11, "Dec" = 12,
           "Long-term" = 13)
    }

  })
  output$ptile_params <- renderUI({
    selectizeInput("ptile_params",
                   label = "Months to plot:",
                   choices = ptile_months_plotting(),
                   selected = 1:length(ptile_months_plotting()),
                   multiple = TRUE)
  })


  ptile_plot_ptile_data <- reactive({
    ptile_data() %>% filter(Month == "Long-term")
  })

  ptile_plot <- function(){

    ptile_data <- data_raw()

    if (input$ptile_datatype == 2) {
      ptile_data <- add_daily_volume(ptile_data) %>%
        mutate(Value = Volume_m3)
    } else if (input$ptile_datatype == 3) {
      ptile_data <- add_daily_yield(ptile_data, basin_area = meta$basin_area) %>%
        mutate(Value = Yield_mm)
    }

    if ("14" %in% input$ptile_params) {
      plot <- plot_flow_duration(data = ptile_data,
                                 #percentiles = seq(from = 0.01,  to = 99.99, by = 0.01),
                                 roll_days = input$ptile_roll_days,
                                 roll_align = input$ptile_roll_align,
                                 water_year_start = as.numeric(input$data_water_year),
                                 start_year = input$data_years_range[1],
                                 end_year = input$data_years_range[2],
                                 exclude_years = as.numeric(input$data_years_exclude),
                                 months = as.numeric(input$ptile_params)[as.numeric(input$ptile_params) %in% 1:12],
                                 include_longterm = ifelse("13" %in% input$ptile_params, TRUE, FALSE),
                                 custom_months = as.numeric(input$ptile_months_cust),
                                 custom_months_label = input$ptile_months_cust_label,
                                 ignore_missing = input$ptile_ign_missing_box)[[1]]
    } else {
      plot <- plot_flow_duration(data = ptile_data,
                                 #percentiles = seq(from = 0.01,  to = 99.99, by = 0.01),
                                 roll_days = input$ptile_roll_days,
                                 roll_align = input$ptile_roll_align,
                                 water_year_start = as.numeric(input$data_water_year),
                                 start_year = input$data_years_range[1],
                                 end_year = input$data_years_range[2],
                                 exclude_years = as.numeric(input$data_years_exclude),
                                 months = as.numeric(input$ptile_params)[as.numeric(input$ptile_params) %in% 1:12],
                                 include_longterm = ifelse("13" %in% input$ptile_params, TRUE, FALSE),
                                 ignore_missing = input$ptile_ign_missing_box)[[1]]
    }

    plot  +
      { if(input$ptile_datatype == 2) ylab("Daily Volumetric Discharge (m3)") } +
      { if(input$ptile_datatype == 3) ylab("Daily Runoff Yield (mm)") } +
      ggplot2::theme(legend.text = ggplot2::element_text(size = 12),
                     axis.title = ggplot2::element_text(size = 15),
                     axis.text = ggplot2::element_text(size = 13))

  }
  output$ptile_plot <- renderPlot({
    ptile_plot()
  })
  output$download_ptile_plot <- downloadHandler(
    filename = function() {paste0(meta$station_name," - Flow Duration Curves.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(ptile_plot())
      dev.off()
    })





  ##### Annual Flows #####

  annual_data <- reactive({

  })

  annual_plot_data <- reactive({
    annual_data() %>% gather(Parameter, Value, 3:ncol(annual_data()))
  })

  output$annual_params <- renderUI({
    selectizeInput("annual_params",
                   label = "Statistics to plot:",
                   choices = unique(annual_plot_data()$Parameter),
                   selected = unique(annual_plot_data()$Parameter),
                   multiple = TRUE)
  })

  annual_plot <- function(){

    plot_data <- annual_plot_data() %>% filter(Parameter %in% input$annual_params)

    ggplot(data = plot_data, aes_string(x = "Year", y = "Value", colour = "Parameter")) +
      geom_line(alpha = 0.5) +
      geom_point() +
      expand_limits(y = 0) +
      ylab("Discharge (cms)") +
      xlab("Year") +
      ggplot2::labs(color = 'Annual Statistics')
  }

  output$annual_plot <- renderPlotly({
    ggplotly(annual_plot())
  })
  output$download_annual_plot <- downloadHandler(
    filename = function() {paste0("Annual Discharge Summary.", input$ann_plottype)},
    content = function(file) {
      ggplot2::ggsave(file, plot = annual_plot(),  width = 11, height = 4, device = input$ann_plottype)
    }
  )

  output$annual_table <- DT::renderDataTable(
    annual_data() %>%
      select(-contains("STATION_NUMBER")) %>%
      mutate_if(is.numeric, funs(round(., 4))),
    rownames = FALSE,
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip')
  )

  output$download_annual_table <- downloadHandler(
    filename = function() {paste0("Annual Discharge Summary.", input$ann_filetype)},
    content = function(file) {
      write_results(data = annual_data(), file)
    }
  )




  ##### Monthly Flows #####





  ##### Daily Flows #####








  ##### Trending #####

  trends_data <- reactive({

    input$trends_compute

    data <- data_raw()
    isolate(compute_annual_trends(data = data,
                                  zyp_method = input$trends_zyp_method,
                                  zyp_alpha = as.numeric(input$trends_alpha),
                                  basin_area = meta$basin_area,
                                  water_year_start = as.numeric(input$data_water_year),
                                  start_year = input$data_years_range[1],
                                  end_year = input$data_years_range[2],
                                  exclude_years = as.numeric(input$data_years_exclude),
                                  annual_percentiles = as.numeric(input$trends_ann_ptiles),
                                  monthly_percentiles = as.numeric(input$trends_mon_ptiles),
                                  stats_days = as.numeric(input$trends_roll_days),
                                  stats_align = input$trends_roll_align,
                                  lowflow_days = as.numeric(input$trends_low_roll_days),
                                  lowflow_align = input$trends_low_roll_align,
                                  timing_percent = as.numeric(input$trends_timing),
                                  normal_percentiles = c(as.numeric(input$trends_normal_lower),as.numeric(input$trends_normal_upper)),
                                  ignore_missing = input$trends_ign_missing_box))
  })


  trends_results_dataframe <- reactive({
    trends_data()[[1]] %>%
      filter(Statistic == trend_to_plot()) %>%
      select(-contains("STATION_NUMBER"), -Statistic) %>%
      gather(Year, Value) %>%
      mutate(Value = round(Value, 3),
             Include = "Yes")
  })

  output$trends_results_data <- DT::renderDataTable(
    trends_results_dataframe(),
    rownames = FALSE,
    selection = list(mode = 'single', selected = 1),
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollY = 350, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip',
                   sDom  = '<"top">lrt<"bottom">ip')
  )

  output$trends_results <- DT::renderDataTable(
    trends_data()[[2]]   %>%
      select(-contains("STATION_NUMBER")) %>%
      mutate_if(is.numeric, funs(round(., 6))),
    rownames = FALSE,
    selection = list(mode = 'single', selected = 1),
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY = 350, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip')
  )

  # output$test <- renderPrint({
  #   as.character(trends_data()$Statistic)[input$trends_results_rows_selected]
  #
  # })
  trend_to_plot <- reactive({

    as.character(trends_data()[[1]]$Statistic)[input$trends_results_rows_selected]

  })


  output$trends_plot <- renderPlotly({

    #data <- trends_data() %>% select(-STATION_NUMBER)

    # plots <- trends_data()[[-1:2]]

    # ggplotly(trends_data()[[paste(trend_to_plot())]])

    ggplotly(trends_data()[[paste(trend_to_plot())]] +
               {if (!is.null(input$trends_results_data_rows_selected))
                 geom_point(aes_string(y=as.numeric(trends_results_dataframe()[input$trends_results_data_rows_selected,2]),
                                       x=as.numeric(trends_results_dataframe()[input$trends_results_data_rows_selected,1])),
                            shape=21,size=2,fill=NA, stroke=2,colour="red")}
    )

  })


  output$testing_rows <- renderPrint({
    as.numeric(c(trends_results_dataframe()[input$trends_results_data_rows_selected,1],
                 trends_results_dataframe()[input$trends_results_data_rows_selected,2]))

  })





  output$trends_code <- renderUI({

    input$trends_compute

    isolate(
      HTML(paste(paste0("compute_annual_trends(station_number = '", input$station_num, "'"),
                 paste0("zyp_method = '", input$trends_zyp_method, "'"),
                 paste0("zyp_alpha = ", as.numeric(input$trends_alpha)),
                 paste0("water_year = ", ifelse(input$data_water_year != "1", "TRUE", "FALSE")),
                 paste0("water_year_start = ", as.numeric(input$data_water_year)),
                 paste0("start_year = ", input$data_years_range[1]),
                 paste0("end_year = ", input$data_years_range[2]),
                 paste0("exclude_years = ", ifelse(length(input$data_years_exclude) == 0, "NULL", list(as.numeric(input$data_years_exclude)))),
                 paste0("annual_percentiles = ", list(as.numeric(input$trends_ann_ptiles))),
                 paste0("monthly_percentiles = ", list(as.numeric(input$trends_mon_ptiles))),
                 paste0("stats_days = ", as.numeric(input$trends_roll_days)),
                 paste0("stats_align = '", input$trends_roll_align, "'"),
                 paste0("lowflow_days = ", list(as.numeric(input$trends_low_roll_days))),
                 paste0("lowflow_align = '", input$trends_low_roll_align, "'"),
                 paste0("timing_percent = ", list(as.numeric(input$trends_timing))),
                 paste0("normal_percentiles = c(", as.numeric(input$trends_normal_lower), ", ", as.numeric(input$trends_normal_upper), ")"),
                 paste0("ignore_missing = ", input$trends_ign_missing_box, ")"),
                 sep = ',<br>'))
    )
  })


  ##### Flow Frequency #####

  # output$freq_station_num <- renderUI({
  #   selectizeInput("freq_station_num", label = "Station Number:",
  #                  choices = stations_list, ### see top of script
  #                  selected = "08HB048",
  #                  options = list(placeholder ="type or select station number", maxOptions = 2420 ))
  # })
  #
  #
  #
  # freq_data_raw <- reactive({
  #   fill_missing_dates(station_number = input$freq_station_num) %>%
  #     add_date_variables(water_year = TRUE,
  #                                water_year_start = as.numeric(input$freq_year_start))
  # })


  output$freq_code <- renderUI({

    input$freq_compute

    #unlist(as.integer(input$freq_months))
    # paste0("compute_annual_frequencies(station_number = '", input$station_num, "', ",
    #        "roll_days = ", input$freq_roll_days, ", ",
    #        "roll_align = '", input$freq_roll_align, "', ",
    #        "use_max = ", input$freq_usemax, ", ",
    #        "use_log = ", input$freq_uselog, ", ",
    #        "prob_plot_position = '", input$freq_prob_plot_position, "', ",
    #        "prob_scale_points = ", "c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)", ", ",
    #        "fit_distr = '", input$freq_fit_distr, "', ",
    #        "fit_distr_method = '", input$freq_fit_distr_method, "', ",
    #        "fit_quantiles = ", "c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)", ", ",
    #        "water_year = ", "TRUE", ", ",
    #        "water_year_start = ", as.numeric(input$data_water_year), ", ",
    #        "start_year = ", input$data_years_range[1], ", ",
    #        "end_year = ", input$data_years_range[2], ", ",
    #        "exclude_years = ", as.numeric(input$data_years_exclude), ", ",
    #        "months = ", list(as.numeric(input$freq_months)), ", ",
    #        "ignore_missing = ", input$freq_ign_missing_box, ")")

    isolate(
      HTML(paste(paste0("compute_annual_frequencies(station_number = '", input$station_num, "'"),
                 paste0("roll_days = ", input$freq_roll_days),
                 paste0("roll_align = '", input$freq_roll_align, "'"),
                 paste0("use_max = ", input$freq_usemax),
                 paste0("use_log = ", input$freq_uselog),
                 paste0("prob_plot_position = '", input$freq_prob_plot_position, "'"),
                 paste0("prob_scale_points = ", "c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)"),
                 paste0("fit_distr = '", input$freq_fit_distr, "'"),
                 paste0("fit_distr_method = '", input$freq_fit_distr_method, "'"),
                 paste0("fit_quantiles = ", "c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)"),
                 paste0("water_year = ", ifelse(input$data_water_year != "1", "TRUE", "FALSE")),
                 paste0("water_year_start = ", as.numeric(input$data_water_year)),
                 paste0("start_year = ", input$data_years_range[1]),
                 paste0("end_year = ", input$data_years_range[2]),
                 paste0("exclude_years = ", ifelse(length(input$data_years_exclude) == 0, "NULL", list(as.numeric(input$data_years_exclude)))),
                 paste0("months = ", list(as.numeric(input$freq_months))),
                 paste0("ignore_missing = ", input$freq_ign_missing_box, ")"),
                 sep = ',<br>'))
    )
  })


  freq_data <- reactive({

    input$freq_compute

    isolate(compute_annual_frequencies(data = data_raw(),
                                       roll_days = input$freq_roll_days,
                                       roll_align = input$freq_roll_align,
                                       use_max = input$freq_usemax,
                                       use_log = input$freq_uselog,
                                       prob_plot_position = input$freq_prob_plot_position,
                                       prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                       fit_distr = input$freq_fit_distr,
                                       fit_distr_method = input$freq_fit_distr_method,
                                       fit_quantiles = c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                                       water_year_start = as.numeric(input$data_water_year),
                                       start_year = input$data_years_range[1],
                                       end_year = input$data_years_range[2],
                                       exclude_years = as.numeric(input$data_years_exclude),
                                       months = as.numeric(input$freq_months),
                                       ignore_missing = input$freq_ign_missing_box))
  })

  output$freq_slider <- renderUI({
    sliderInput("freq_slider",
                label = "Start and end years:",
                min = ifelse(as.numeric(input$freq_year_start) == 1, min(freq_data_raw()$Year), min(freq_data_raw()$WaterYear)),
                max = ifelse(as.numeric(input$freq_year_start) == 1, max(freq_data_raw()$Year), max(freq_data_raw()$WaterYear)),
                value = c(ifelse(as.numeric(input$freq_year_start) == 1, min(freq_data_raw()$Year), min(freq_data_raw()$WaterYear)),
                          ifelse(as.numeric(input$freq_year_start) == 1, max(freq_data_raw()$Year), max(freq_data_raw()$WaterYear))),
                dragRange = TRUE,
                sep = "")
  })

  freq_plot_data <- reactive({
    freq_data() %>% gather(Parameter, Value, 3:ncol(freq_data()))
  })

  output$freq_exclude <- renderUI({
    selectizeInput("freq_exclude",
                   label = "Years to exclude:",
                   choices = seq(from = input$freq_slider[1], to = input$freq_slider[2], by = 1),
                   selected = NULL,
                   multiple = TRUE)
  })

  # freq_plot <- function(){
  #   # plot <- plot_annual_stats(station_number = "08HB048")
  #
  #
  #   print(plot)
  # }
  #
  # output$freq_plot <- renderPlot({
  #   freq_plot()
  # })
  #
  output$freq_Q_stat <- renderDataTable({
    freq_data()$Freq_Analysis_Data
  })
  output$freq_plotdata <- renderDataTable({
    freq_data()$Freq_Plot_Data
  })
  output$freq_fitted_quantiles <- renderDataTable({
    freq_data()$Freq_Fitted_Quantiles
  })
  output$freq_freqplot <- plotly::renderPlotly({
    ggplotly(freq_data()$Freq_Plot)
  })
  output$freq_fit <- renderPrint({
    freq_data()$Freq_Fitting
  })


  ##### OLDER CODE #####


  dailyData <- reactive({

    #timeseries <- timeseriesData()
    daily.data <- data_raw() %>%
      group_by(DayofYear)%>%
      filter(DayofYear < 366) %>% # removes any day 366 during leap years; i.e. only the first 365 days of each year are summarized
      summarize(Mean=mean(Value, na.rm=TRUE),
                Minimum=min(Value, na.rm=TRUE),
                FifthPercentile=quantile(Value,.05, na.rm=TRUE),
                TwentyFifthPercentile=quantile(Value,.25, na.rm=TRUE),
                Median=median(Value, na.rm=TRUE),
                SeventyFifthPercentile=quantile(Value,.75, na.rm=TRUE),
                NinetyFifthPercentile=quantile(Value,.95, na.rm=TRUE),
                Maximum=max(Value, na.rm=TRUE))

    if (input$yearCheckDaily) {
      flow.Year <- data_raw() %>% filter(analysisYear==input$yearDaily & analysisDOY <366) %>% select(analysisDOY,Value) %>% rename("yearValue"=Value)
      daily.data <- merge(daily.data,flow.Year, by = "analysisDOY", all.x = TRUE)
    }
    daily.data <- as.data.frame(daily.data)
  })

  output$yearSelectDaily <- renderUI({
    sliderInput("yearDaily", label = "Select year of daily discharge to plot:",value=yearData()$minYear,
                min=min(data_raw()$analysisYear), max=max(data_raw()$analysisYear),sep = "")#yearData()$minYear
  })

  dailyPlot <- function(){

    daily.plot <- ggplot(dailyData(),aes_string(x="DayofYear")) +
      geom_ribbon(aes(ymin=Minimum,ymax=Maximum,fill = "Max-Min Range of Flow"))+
      geom_ribbon(aes(ymin=FifthPercentile,ymax=NinetyFifthPercentile,fill = "Range of 90% of Flow"))+
      geom_ribbon(aes(ymin=TwentyFifthPercentile,ymax=SeventyFifthPercentile,fill = "Range of 50% of Flow"))+
      geom_line(aes(y=Median, colour="Median Flow"), size=.5)+
      geom_line(aes(y=Mean, colour="Mean Flow"), size=.5) +
      {if(input$yearCheckDaily)geom_line(aes(y=yearValue,colour="YEAR"))}+
      scale_fill_manual(values = c("Max-Min Range of Flow" = "lightblue2" ,"Range of 90% of Flow" = "lightblue3","Range of 50% of Flow" = "lightblue4")) +
      scale_color_manual(values = c("Mean Flow" = "paleturquoise", "Median Flow" = "dodgerblue4"),
                         labels = c("Mean Flow", "Median Flow")) +
      {if(input$yearCheckDaily)  scale_color_manual(values = c("Mean Flow" = "paleturquoise", "Median Flow" = "dodgerblue4","YEAR" = "red"),
                                                    labels = c("Mean Flow", "Median Flow","YEAR"=input$yearDaily))}+
      #scale_x_date(date_labels = "%b", date_breaks = "1 month",expand=c(0,0))+
      scale_y_continuous(expand = c(0, 0)) +
      {if(input$logDaily)scale_y_log10(expand = c(0, 0))}+
      {if(input$logDaily)annotation_logticks(base= 10,"left",size=0.6,short = unit(.14, "cm"), mid = unit(.3, "cm"), long = unit(.5, "cm"))}+
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=15),
            axis.ticks = element_line(size=.1),
            axis.ticks.length=unit(0.1,"cm"),
            axis.title.y=element_text(margin=margin(0,0,0,0)),
            plot.title = element_text(size=15,hjust = 0.5),
            #panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size=.1),
            panel.background = element_rect(fill = "grey94"),
            legend.position = "top", legend.title = element_blank(),
            legend.text = element_text(size=13),
            legend.box = "vertical",
            legend.key.size = unit(0.4,"cm"),
            legend.margin=unit(.2, "cm")) +
      guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))+
      xlab(NULL)+
      ylab("Discharge (cms)")+
      ggtitle(paste0("Daily Stream Discharge - ",meta$station_name," (",input$yearRange[1],"-",input$yearRange[2],")"))
    print(daily.plot)
  }

  #structure to show the plot interactively
  output$dailyPlot <- renderPlot({
    dailyPlot()
  })

  output$downloadDailyPlot <- downloadHandler(
    filename = function() {paste0(meta$station_name," - Daily Discharge Summary"," (",input$yearRange[1],"-",input$yearRange[2],").png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(dailyPlot())
      dev.off()
    })

  dailyTable <- reactive({
    dailyTable <- dailyData()# %>%
    #   rename("Date"=analysisDate,"Day of Year"=analysisDOY,"5th Percentile"=FifthPercentile,"25th Percentile"=TwentyFifthPercentile,"75th Percentile"=SeventyFifthPercentile,"95th Percentile"=NinetyFifthPercentile)
    # if (input$yearCheckDaily) {
    #   names(dailyTable)[names(dailyTable) == 'yearValue'] <- paste(input$yearDaily)
    # }
    # dailyTable$"Date" <- format(as.Date(dailyTable$"Date"),format="%b-%d")
    # dailyTable[,c(3:10)] <- round(dailyTable[,c(3:10)],3)
    # dailyTable <- as.data.frame(dailyTable)
  })

  output$dailyTable <- renderDataTable({
    dailyTable()
  })

  output$downloadDailyTable <- downloadHandler(
    filename = function() {paste0(meta$station_name,
                                  " - Daily Discharge Summary"," (",
                                  input$yearRange[1],"-",input$yearRange[2],
                                  ").csv")},
    content = function(file) {
      write.csv(dailyTable(),file, row.names = FALSE)
    })
}







  # output$station_num <- renderUI({
  #   selectizeInput("station_num", label = "Station Number:",
  #                  choices = stations_list, ### see top of script
  #                  selected = "08HB048",
  #                  options = list(placeholder ="type or select station number", maxOptions = 2420 ))
  # })
  #


  # data_raw <- reactive({
  #   fill_missing_dates(station_number = input$station_num) %>%
  #     add_date_variables(water_year = TRUE,
  #                                water_year_start = as.numeric(input$ann_year_start))
  # })


  # output$ann_basinarea <- renderUI({
  #   numericInput("ann_basinarea",
  #                label = "Basin area (sq. km):",
  #                value =  round(suppressMessages(tidyhydat::hy_stations(station_number = input$station_num)) %>% pull(DRAINAGE_AREA_GROSS),3),
  #                min = 0, step = 0.1)
  # })















  # output$download_annual_table <- downloadHandler(
  #   filename = function() {paste0("Annual Discharge Summary.", input$ann_filetype)},
  #   content = function(file) {
  #     write_results(data = annual_data(), file)
  #
