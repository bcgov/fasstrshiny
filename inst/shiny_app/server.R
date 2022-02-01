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
    select_plot_options(data = data_raw(), id = "data", input,
                        include = c("log", "daterange"))
  })

  ## Summary ------------------------------------------------------------------
  output$ui_sum <- renderUI({
    build_ui(id = "sum", input,
             include = c("discharge", "miss_allowed"))
  })

  # Plot options
  output$ui_sum_plot_options <- renderUI({
    req(input$data_years_range)

    select_plot_options(id = "sum", input,
                        include = c("log", "year_add", "dates_add"))
    # Add inner/outer percentiles?
  })

  # Enable/disable year_add and dates_add
  observe({
    if(input$sum_type %in% c("Long-term", "Daily")) {
      enable("sum_year_add")
    } else {
      disable("sum_year_add")
    }

    if(input$sum_type == "Daily") enable("sum_dates_add") else disable("sum_dates_add")
  }) %>%
    bindEvent(input$sum_type)

  # Table options
  output$ui_sum_table_options <- renderUI({
    select_table_options(id = "sum", input)
  })

  ## Summary - Flow ----------------------------------------------------------
  output$ui_sumfl <- renderUI({
    build_ui(id = "sumfl", input,
             include = c("discharge", "missing", "custom_months"))
  })

  # Plot options
  output$ui_sumfl_plot_options <- renderUI({
    select_plot_options(data = data_raw(), id = "sumfl", input,
                        include = "log")
  })

  ## Cumulative ----------------------------------------------------------

  # Plot options
  output$ui_cum_plot_options <- renderUI({
    select_plot_options(data = data_raw(), id = "cum", input, include = "log")
  })

  # Table options
  output$ui_cum_table_options <- renderUI({
    select_table_options(data = data_raw(), id = "cum", input,
                         include = "percentiles")
  })

  # Enable/disable seasons
  observe({
    req(input$cum_seasons)
    if(input$cum_type != "Annual") {
      disable("cum_seasons")
    } else enable("cum_seasons")
  }) %>%
    bindEvent(input$cum_type)


  ## AH - Flow timing --------------------------------------------------------
  # None

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
                  value = input$opts_allowed, step = 5, min = 0, max = 100))
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

    updateTabsetPanel(session, inputId = "data_tabs", selected = "data_plot")

    # Save unevaluated code for code tab
    code$data_raw <- d

    eval(parse(text = d)) # Evaluate and create flow_data
  }) %>%
    bindEvent(input$data_load)

  ## Plot ----------------
  output$data_plot <- renderPlotly({
    req(data_raw(),
        !is.null(input$data_log),
        input$data_daterange,
        input$data_years_range,
        input$data_water_year)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_flow_data", "flow_data", id = "data", input,
      params = c("log", "daterange"),
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
    g <- create_fun("plot_missing_dates", data = "flow_data",
                    id = "screen", input, end = "[[1]]")

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
                               dom = 'Brtip'))
  })

  ## R Code -----------------
  output$screen_code <- renderText({
    code_format(code, id = "screen")
  })



  # Summary Statistics - General ---------------------------------------

  ## Plot --------------------
  output$sum_plot <- renderPlot({
    req(data_raw(), input$sum_type,
        !is.null(input$sum_log), !is.null(input$sum_year_add))

    flow_data <- data_raw()

    e <- NULL
    if(input$sum_type %in% c("Long-term", "Daily") & input$sum_year_add != "") {
      e <- glue("add_year = {input$sum_year_add}")
    }

    end <- "[[1]]"
    if(input$sum_type == "Daily" & !is.null(input$sum_dates_add)){
      dts <- glue("as.Date(c(\"{glue_collapse(input$sum_dates_add, sep = '\", \"')}\"))")
      labs <- glue("c(\"{glue_collapse(format(as.Date(input$sum_dates_add), '%b-%d'), ",
                   "sep = '\", \"')}\")")
      end <- glue("{end} + ",
                  "  geom_vline(xintercept = {dts}, colour = 'grey20') +",
                  "  annotate(geom = 'text', x = {dts}, y = Inf, vjust = 2, ",
                  "           hjust = 1.05, label = {labs})")
    }

    g <- switch(input$sum_type,
                "Long-term" = "plot_longterm_daily_stats",
                "Annual" = "plot_annual_stats",
                "Monthly" = "plot_monthly_stats",
                "Daily" = "plot_daily_stats") %>%
      create_fun("flow_data", id = "sum", input,
                 params = c(if_else(input$sum_type %in% c("Long-term", "Daily"),
                                    "missing", "allowed"),
                            "log"),
                 extra = e,
                 end = end)

    code$sum_plot <- g

    eval(parse(text = g))
  })

  ## MAD -----------------------
  output$sum_mad <- render_gt({
    req(input$sum_discharge, input$sum_mad)

    flow_data <- data_raw()

    t <- create_fun(
      fun = "calc_longterm_mean",
      data = "flow_data", id = "sum", input,
      params = "complete",
      extra = glue("percent_MAD = c({glue_collapse(input$sum_mad, sep = ',')})"))

    code$sum_mad <- t

    parse(text = t) %>%
      eval() %>%
      gt() %>%
      fmt_number(columns = where(is.numeric), decimals = 4)
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
                 params = c("percentiles",
                            case_when(
                              input$sum_type == "Daily" ~ "missing",
                              input$sum_type == "Long-term" ~
                                c("missing", "custom_months", "custom_months_label"),
                              TRUE ~ "allowed")))


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
    code_format(code, id = "sum")
  })


  # Summary Statistics - Flow ---------------------------------------

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
  output$sumfl_plot <- renderPlot({
    req(data_raw(), !is.null(input$sumfl_log))

    flow_data <- data_raw()

    # missing arguments
    # - include_longterm

    g <- create_fun(
      fun = "plot_flow_duration", data = "flow_data", id = "sumfl", input,
      params = c("custom_months", "custom_months_label",
                 "missing", "log"),
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
    code_format(code, id = "sumfl")
  })

  # Cumulative ---------------------------------------
  ## Plot --------------------
  output$cum_plot <- renderPlot({
    req(data_raw(), input$cum_type)

    flow_data <- data_raw()

    # Patchwork plot design
    d <- "AC\\nBC"

    e <- glue("use_yield = {input$cum_discharge}")
    if(input$cum_type == "Annual") {
      p <- NULL
      e <- glue("{e}, include_seasons = {input$cum_seasons}")
      end <- glue("%>% wrap_plots(nrow = 2, byrow = FALSE, design = '{d}')")
    } else {
      p <- "log"
      end <- "[[1]]"
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

    eval(parse(text = g))
  })


  ## Table -----------------------
  output$cum_table <- DT::renderDT({
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
    code_format(code, id = "cum")
  })


  # AH - Flow timing ---------------------------------------
  ## Plot --------------------
  output$ahft_plot <- renderPlot({
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

    eval(parse(text = g))
  })


  ## Table -----------------------
  output$ahft_table <- DT::renderDT({
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
    code_format(code, id = "ahft")
  })


  # AH - Low Flows ---------------------------------------
  ## Plot --------------------
  output$ahlf_plot <- renderPlot({
    req(data_raw(), input$ahlf_discharge)

    flow_data <- data_raw()

    g <- create_fun(
      "plot_annual_lowflows", data = "flow_data",
      id = "ahlf", input,
      params = c("discharge", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$ahlf_roll, sep = ',')})"),
      end = " %>% wrap_plots()")

    code$ahlf_plot <- g

    eval(parse(text = g))
  })


  ## Table -----------------------
  output$ahlf_table <- DT::renderDT({
    req(data_raw(), input$ahlf_discharge)

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_lowflows", data = "flow_data",
      id = "ahlf", input,
      params = c("discharge", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$ahlf_roll, sep = ',')})"))

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
    code_format(code, id = "ahlf")
  })

  # AH - Peaks ---------------------------------------

  ## Table -----------------------
  output$ahp_table <- DT::renderDT({
    req(data_raw(), input$ahp_discharge)

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_peaks", data = "flow_data",
      id = "ahp", input,
      params = c("discharge", "missing", "allowed"),
      params_ignore = "roll_days",
      extra = glue("roll_days = ",
                   "c({glue_collapse(input$ahp_roll, sep = ',')})"))

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
    code_format(code, id = "ahp")
  })

  # AH - Days outside normal ---------------------------------------
  ## Plot --------------------
  output$ahon_plot <- renderPlot({
    req(data_raw(), input$ahon_discharge)
    validate(
      need(length(input$ahon_percentiles) == 2,
           paste0("Can only have 2 percentiles defining lower and upper ",
                  "bounds of the normal range")))

    flow_data <- data_raw()

    g <- create_fun(
      "plot_annual_outside_normal", data = "flow_data",
      id = "ahon", input,
      extra = glue("normal_percentiles = ",
                   "c({glue_collapse(input$ahon_percentiles, sep = ',')})"),
      end = "[[1]]")

    code$ahon_plot <- g

    eval(parse(text = g))
  })


  ## Table -----------------------
  output$ahon_table <- DT::renderDT({
    req(data_raw(), input$ahon_discharge)
    validate(
      need(length(input$ahon_percentiles) == 2,
           paste0("Can only have 2 percentiles defining lower and upper ",
                  "bounds of the normal range")))

    flow_data <- data_raw()

    t <- create_fun(
      "calc_annual_outside_normal", data = "flow_data",
      id = "ahon", input,
      params = "discharge",
      extra = glue("normal_percentiles = ",
                   "c({glue_collapse(input$ahon_percentiles, sep = ',')})"))

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
    code_format(code, id = "ahon")
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
    validate(need(input$at_compute,
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
  output$at_plot <- renderggiraph({
    at_trends()[[at_stat()]] %>%
      to_ggiraph(value = at_stat())
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
    validate(need(input$at_compute,
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
    code_format(code, id = "at")
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

    validate(need(all(!is.na(text_to_num(input$vf_prob_scale))),
                  "Probabilies to plot must be a comma separated list of numbers"))

    flow_data <- data_raw()

    #
    # roll_align


    # Define parameters
    p <- c(
      glue("exclude_years = c({glue_collapse(input$vf_years_exclude, sep = ', ')})"),
      glue("use_max = {input$vf_use_max}"),
      glue("use_log = {input$vf_log}"),
      glue("roll_days = c({glue_collapse(input$vf_roll_extra, sep = ', ')})"),
      glue("prob_plot_position = '{input$vf_prob_plot}'"),
      glue("prob_scale_points = c({input$vf_prob_scale})"),
      glue("fit_distr = '{input$vf_fit_distr}'"),
      glue("fit_quantiles = c({glue_collapse(input$vf_quantiles, sep = ', ')})"),
      glue("plot_curve = {input$vf_plot_curve}")) %>%
      glue_collapse(sep = ", ")

    r <- create_fun(
      "compute_annual_frequencies",
      data = "flow_data", id = "vf", input,
      params = c("discharge", "allowed"),
      params_ignore = c("roll_days", "years_exclude"),
      extra = p)

    code$vf_data <- r

    eval(parse(text = r))
  }) %>%
    bindEvent(input$vf_compute)

  ## Plot --------------------
  output$vf_plot <- renderGirafe({
    validate(need(input$vf_compute,
                  "Choose your settings and click 'Compute Analysis'"))

    vf_freqs()[["Freq_Plot"]] %>%
      to_ggiraph(type = "flow")
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
    validate(need(input$vf_compute,
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
    validate(need(input$vf_compute,
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
    validate(need(vf_freqs(),
                  "Choose your settings and click 'Compute Analysis'"))
    req(input$vf_day)
    vf_freqs()[["Freq_Fitting"]][[input$vf_day]] %>%
      gg_fitdistr(title = input$vf_day)
  })



  ## R Code -----------------
  output$vf_code <- renderText({
    code_format(code, id = "vf")
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

    validate(need(all(!is.na(text_to_num(input$hp_prob_scale))),
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
      glue("fit_quantiles = c({glue_collapse(input$hp_quantiles, sep = ', ')})"),
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
  output$hp_plot <- renderPlot({
    validate(need(input$hp_compute,
                  "Choose your settings and click 'Compute Analysis'"))
    req(hp_freqs())

    hp_freqs()[["Freq_Plot"]]
  })


  ## Table -----------------------
  output$hp_table <- DT::renderDT({
    validate(need(input$hp_compute,
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
    validate(need(hp_freqs(),
                  "Choose your settings and click 'Compute Analysis'"))
    hp_freqs()[["Freq_Fitting"]][[11]] %>%
      gg_fitdistr(title = "")
  })



  ## R Code -----------------
  output$hp_code <- renderText({
    code_format(code, id = "hp")
  })


}


# TO ADD ----- Functions to add from fasstr: ------------------
# - calc_longterm_monthly_stats
# - plot_longterm_monthly_stats
# - plot_annual_means
# - compute_frequency_quantile ??? only gives single value


