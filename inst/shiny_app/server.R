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

  # UI elements ---------------------------------------
  output$ui_opts <- build_ui(id = "opts", input, define_options = TRUE,
                             include = c("discharge", "rolling", "months",
                                         "percentiles", "missing"))

  output$ui_data_station_num <- renderUI({
    textInput("data_station_num", label = "Station Number",
              value = "08HB048",
              placeholder = "type station number or select from map")
  })

  observeEvent(input$data_map_station, {
    req(input$data_map_marker_click)

    updateTextInput(session, "data_station_num",
                    value = input$data_map_marker_click$id)

  })

  output$ui_sum <- build_ui(id = "sum", input,
                             include = c("discharge", "rolling", "months",
                                         "percentiles", "missing"))

  output$ui_sum_flow <- build_ui(id = "sum_flow", input,
                                 include = c("discharge", "rolling", "months",
                                             "missing"))


  # Data - Loading ---------------


  ## Map -------------------------
  output$data_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(data = stations, lng = ~LONGITUDE, lat = ~LATITUDE,
                       layerId = ~ STATION_NUMBER,
                       radius = 4, fillOpacity = 1, stroke = FALSE,
                       label = ~ STATION_NUMBER,
                       popup = ~glue("Station Name: {STATION_NAME}<br>",
                                     "Station Number: {STATION_NUMBER}"))
  })


  # Raw daily discharge data
  data_raw <- eventReactive(input$data_select, {
    req(input$year_start)

    if (input$data_source == "HYDAT") {
      d <- fill_missing_dates(station_number = input$data_station_num) %>%
        add_date_variables(water_year_start = as.numeric(input$year_start))
    } else {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)

      csv_file <- as.data.frame(read.csv(inFile$datapath, header = T, sep = ","))

      d <- fill_missing_dates(data = csv_file) %>%
        add_date_variables(water_year_start = as.numeric(input$year_start))
    }

    updateTabsetPanel(session, inputId = "data_tabs", selected = "data_plot")
    d
  })

# Station name UI
output$station_name <- renderUI({
  textInput('station_name', label = "Station/stream name:", placeholder = "ex. Mission Creek",
            value = ifelse(input$data_source == "HYDAT",
                           paste0(suppressMessages(tidyhydat::hy_stations(station_number = input$station_num)) %>% pull(STATION_NAME),
                                  " (",input$station_num,")"),
                           ""))
})

  # Basin area UI
  output$basinarea <- renderUI({
    numericInput("basinarea",
                 label = "Basin area (sq. km):",
                 value = ifelse(input$data_source == "HYDAT",
                                round(suppressMessages(tidyhydat::hy_stations(station_number = input$station_num)) %>% pull(DRAINAGE_AREA_GROSS),3),
                                NA),
                 min = 0, step = 0.1)
  })

  # Year selection/slider UI
  output$years_range <- renderUI({
    sliderInput("years_range", label = "Select start and end years to summarize:",
                min = min(data_raw()$WaterYear),
                max = max(data_raw()$WaterYear),
                value = c(min(data_raw()$WaterYear), max(data_raw()$WaterYear)),
                dragRange = TRUE, sep = "")
  })

  # Exclude years selection
  output$years_exclude <- renderUI({
    selectizeInput("years_exclude",
                   label = "Years to exclude:",
                   choices = seq(from = input$years_range[1], to = input$years_range[2], by = 1),
                   selected = NULL,
                   multiple = TRUE)
  })

  # Daily timeseries outputs
  output$dateRange <- renderUI({
    dateRangeInput("dateRange", "Select start and end date of plot:", format = "yyyy-mm-dd", startview = "month",
                   start = min(data_raw()$Date), end = max(data_raw()$Date))#"1950-01-01",end = "2000-12-31")
  })

  output$data_timeseries <- renderPlotly({
    req(data_raw())
    ggplotly(plot_timeseries(data = data_raw(), input))
  })

  output$downloadtimeseries_plot <- downloadHandler(
    filename = function() {paste0(input$station_name," - Full Time Series.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(timeseries_plot())
      dev.off()
    })

  output$timeseries_data <- DT::renderDataTable({
    # data_raw() %>%
    #   select(-dplyr::contains("STATION_NUMBER"), -dplyr::contains("Parameter"), -Month, Month = MonthName) %>%
    #   rename("Day of Year" = DayofYear, "Water Year" = WaterYear, "Day of Water Year" = WaterDayofYear) %>%
    #   mutate(Value = round(Value, 4))
    data <- data_raw()
    data <- select(data,-dplyr::contains("STATION_NUMBER"), -dplyr::contains("Parameter"), -Month, Month = MonthName)
    data <- rename(data,"Day of Year" = DayofYear, "Water Year" = WaterYear, "Day of Water Year" = WaterDayofYear)
    data <- mutate(data,Value = round(Value, 4))
    data}
    ,
    rownames = FALSE,
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip')
  )


  ##### Data Screening #####

  ##### Summary plot
  summaryData <- reactive({
    screen_flow_data(data = data_raw(),
                     start_year = input$years_range[1],
                     end_year = input$years_range[2],
                     water_year_start = as.numeric(input$year_start))
  })

  summaryPlot <- function(){

    plotdata <- summaryData() %>%
      select(Year, Minimum, Maximum, Mean, StandardDeviation) #%>%  gather(Statistic,Value,2:5)

    ggplot(data = plotdata, aes_string(x = "Year", y = input$summaryY)) +
      ggtitle(paste0("Annual Daily ", input$summaryY, " - ", input$station_name)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_line(colour = "dodgerblue4") +
      geom_point(colour = "firebrick3", size = 2) +
      #facet_wrap(~Statistic, ncol=2, scales="free_y")+
      xlab("Year") +
      theme_bw() +
      { if(as.numeric(input$year_start) != 1) xlab("Water Year")} +
      ylab("Discharge (cms)") +
      theme(axis.title = element_text(size = 15),
            plot.title = element_text(size = 15, hjust = 0.5),
            axis.text = element_text(size = 13))
  }
  output$summaryPlot <- renderPlotly({
    ggplotly(summaryPlot())
  })
  output$downloadSummaryPlot <- downloadHandler(
    filename = function() {paste0(input$station_name," - Full Time Series Annual",input$summaryY,".png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(summaryPlot())
      dev.off()
    })

  output$summary_table <- DT::renderDataTable(
    summaryData() %>%
      select(-dplyr::contains("STATION_NUMBER")) %>%
      rename("Total Days" = n_days, "Total Flow Days" = n_Q, "Total Missing Days" = n_missing_Q,
             "Jan Missing Days" = Jan_missing_Q, "Feb Missing Days" = Feb_missing_Q,
             "Mar Missing Days" = Mar_missing_Q, "Apr Missing Days" = Apr_missing_Q,
             "May Missing Days" = May_missing_Q, "Jun Missing Days" = Jun_missing_Q,
             "Jul Missing Days" = Jul_missing_Q, "Aug Missing Days" = Aug_missing_Q,
             "Sep Missing Days" = Sep_missing_Q, "Oct Missing Days" = Oct_missing_Q,
             "Nov Missing Days" = Nov_missing_Q, "Dec Missing Days" = Dec_missing_Q,
             "Standard Deviation" = StandardDeviation)  %>%
      mutate_if(is.numeric, funs(round(., 4))),
    rownames = FALSE,
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip')
  )

  output$download_summary_table <- downloadHandler(
    filename = function() {paste0(input$station_name," - data screening.csv")},
    content = function(file) {
      write.csv(summaryData(), file, row.names = FALSE)
    })


  ##### Missing Data Table

  missing_plot <- function(){

    plot_missing_dates(data = data_raw(),
                       start_year = input$years_range[1],
                       end_year = input$years_range[2],
                       water_year_start = as.numeric(input$year_start),
                       months = as.numeric(input$availability_months))[[1]]
    #
    #
    # plotdata <- summaryData() %>%
    #   select(-dplyr::contains("STATION_NUMBER"), -Minimum, - Mean, -Median, -Maximum, -StandardDeviation,
    #          -n_days, -n_Q, -n_missing_Q) %>%
    #   gather(Month, Value, 2:13) %>%
    #   mutate(Month = substr(Month, 1, 3))
    #
    # if (input$year_start == 1) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
    #                                                       "Aug", "Sep", "Oct", "Nov", "Dec"))
    # } else if (input$year_start == 2) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    #                                                       "Sep", "Oct", "Nov", "Dec", "Jan"))
    # } else if (input$year_start == 3) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
    #                                                       "Oct", "Nov", "Dec", "Jan", "Feb"))
    # } else if (input$year_start == 4) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
    #                                                       "Nov", "Dec", "Jan", "Feb", "Mar"))
    # } else if (input$year_start == 5) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
    #                                                       "Dec", "Jan", "Feb", "Mar", "Apr"))
    # } else if (input$year_start == 6) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    #                                                       "Jan", "Feb", "Mar", "Apr", "May"))
    # } else if (input$year_start == 7) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan",
    #                                                       "Feb", "Mar", "Apr", "May", "Jun"))
    # } else if (input$year_start == 8) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb",
    #                                                       "Mar", "Apr", "May","Jun", "Jul"))
    # } else if (input$year_start == 9) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
    #                                                       "Apr", "May", "Jun", "Jul", "Aug"))
    # } else if (input$year_start == 10) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr",
    #                                                       "May", "Jun", "Jul", "Aug", "Sep"))
    # } else if (input$year_start == 11) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",
    #                                                       "Jun", "Jul", "Aug", "Sep", "Oct"))
    # } else if (input$year_start == 12) {
    #   plotdata$Month <- factor(plotdata$Month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    #                                                       "Jul", "Aug", "Sep", "Oct", "Nov"))
    # }
    #
    #
    # ggplot(data = plotdata, aes_string(x = "Year", y = "Value")) +
    #   # ggtitle(paste0("Annual Daily ", input$summaryY, " - ", input$station_name)) +
    #   theme(plot.title = element_text(hjust = 0.5)) +
    #   geom_line(colour = "dodgerblue4") +
    #   geom_point(colour = "firebrick3", size = 2) +
    #   facet_wrap(~Month, ncol = 3, scales = "fixed") +
    #   #facet_wrap(~Statistic, ncol=2, scales="free_y")+
    #   xlab("Year") +
    #   theme_bw() +
    #   { if(as.numeric(input$year_start) != 1) xlab("Water Year")} +
    #   ylab("Number of Days") +
    #   theme(axis.title = element_text(size = 15),
    #         plot.title = element_text(size = 15, hjust = 0.5),
    #         axis.text = element_text(size = 13))
  }
  output$missing_plot <- renderPlotly({
    ggplotly(missing_plot())
  })

  output$download_missing_plot <- downloadHandler(
    filename = function() {paste0(input$station_name," - missing data plot.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(missing_plot())
      dev.off()
    })


  ##### Long-term Flows #####

  # Summary Statistics
  lt_data <- reactive({

    lt_data <- data_raw()

    if (input$lt_datatype == 2) {
      lt_data <- add_daily_volume(lt_data) %>%
        mutate(Value = Volume_m3)
    } else if (input$lt_datatype == 3) {
      lt_data <- add_daily_yield(lt_data, basin_area = input$basinarea) %>%
        mutate(Value = Yield_mm)
    }

    calc_longterm_daily_stats(data = lt_data,
                              percentiles = as.numeric(input$lt_ptiles),
                              roll_days = input$lt_roll_days,
                              roll_align = input$lt_roll_align,
                              water_year_start = as.numeric(input$year_start),
                              start_year = input$years_range[1],
                              end_year = input$years_range[2],
                              exclude_years = as.numeric(input$years_exclude),
                              custom_months = as.numeric(input$lt_months),
                              custom_months_label = input$lt_months_label,
                              ignore_missing = input$lt_ign_missing_box)
  })

  output$lt_table <- DT::renderDataTable(
    lt_data() %>% select(-dplyr::contains("STATION_NUMBER")) %>%
      mutate_if(is.numeric, funs(round(., 4))),
    rownames = FALSE,
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip')
  )
  output$download_lt_table <- downloadHandler(
    filename = function() {paste0(input$station_name," - Long-term Statistics.csv")},
    content = function(file) {
      write.csv(lt_data(), file, row.names = FALSE)
    })


  lt_plot_data <- reactive({
    lt_data() %>% gather(Parameter, Value, 3:ncol(lt_data()))
  })

  output$lt_params <- renderUI({
    selectizeInput("lt_params",
                   label = "Statistics to plot:",
                   choices = unique(lt_plot_data()$Parameter),
                   selected = unique(lt_plot_data()$Parameter),
                   multiple = TRUE)
  })


  lt_plot_lt_data <- reactive({
    lt_data() %>% filter(Month == "Long-term")
  })

  lt_plot <- function(){

    plot_data <- lt_plot_data() %>%
      filter(Parameter %in% input$lt_params) %>%
      filter(Month != "Long-term") %>%
      mutate(Month = match(Month, month.abb))

    ggplot(data = plot_data, aes_string(x = "Month", y = "Value", colour = "Parameter")) +
      geom_line() +
      geom_point() +
      geom_hline(aes(yintercept=lt_plot_lt_data()$Mean, colour = "LTMean"), linetype = 2)+
      geom_hline(aes(yintercept=lt_plot_lt_data()$Median, colour = "LTMedian"))+
      expand_limits(y = 0) +
      ylab("Discharge (cms)") +
      { if(input$lt_datatype == 2) ylab("Volumetric Discharge (m3)") } +
      { if(input$lt_datatype == 3) ylab("Runoff Yield (mm)") } +
      xlab("Month") +
      scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::labs(color = 'Long-term Statistics') +
      {if(input$lt_logQ) scale_y_log10(expand = c(0, 0)) } +
      {if(input$lt_logQ) annotation_logticks(base= 10,"left",size=0.6,short = unit(.14, "cm"), mid = unit(.3, "cm"), long = unit(.5, "cm"))}+
      {if(!is.null(input$lt_plot_title)) ggtitle(paste(input$lt_plot_title))} +
      ggplot2::theme(legend.position = "right",
                     legend.spacing = ggplot2::unit(0, "cm"),
                     legend.justification = "top",
                     legend.text = ggplot2::element_text(size = 9),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10))
  }
  output$lt_plot <- renderPlot({
    lt_plot()
  })

  output$download_lt_plot <- downloadHandler(
    filename = function() {paste0(input$station_name," - Long-term Statistics.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(lt_plot())
      dev.off()
    })



  # Flow Duration and Percentiles

  ptile_data <- reactive({

    ptile_data <- data_raw()

    if (input$ptile_datatype == 2) {
      ptile_data <- add_daily_volume(ptile_data) %>%
        mutate(Value = Volume_m3)
    } else if (input$ptile_datatype == 3) {
      ptile_data <- add_daily_yield(ptile_data, basin_area = input$basinarea) %>%
        mutate(Value = Yield_mm)
    }

    calc_longterm_daily_stats(data = ptile_data,
                              percentiles = seq(from = 1,  to = 99, by = 1),
                              roll_days = input$ptile_roll_days,
                              roll_align = input$ptile_roll_align,
                              water_year_start = as.numeric(input$year_start),
                              start_year = input$years_range[1],
                              end_year = input$years_range[2],
                              exclude_years = as.numeric(input$years_exclude),
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
    filename = function() {paste0(input$station_name," - Long-term Percentiles.csv")},
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
      ptile_data <- add_daily_yield(ptile_data, basin_area = input$basinarea) %>%
        mutate(Value = Yield_mm)
    }

    if ("14" %in% input$ptile_params) {
      plot <- plot_flow_duration(data = ptile_data,
                                 #percentiles = seq(from = 0.01,  to = 99.99, by = 0.01),
                                 roll_days = input$ptile_roll_days,
                                 roll_align = input$ptile_roll_align,
                                 water_year_start = as.numeric(input$year_start),
                                 start_year = input$years_range[1],
                                 end_year = input$years_range[2],
                                 exclude_years = as.numeric(input$years_exclude),
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
                                 water_year_start = as.numeric(input$year_start),
                                 start_year = input$years_range[1],
                                 end_year = input$years_range[2],
                                 exclude_years = as.numeric(input$years_exclude),
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
    filename = function() {paste0(input$station_name," - Flow Duration Curves.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(ptile_plot())
      dev.off()
    })
  ##### Annual Flows #####

  annual_data <- reactive({

    ann_data <- data_raw()

    if (input$ann_datatype == 2) {
      ann_data <- add_daily_volume(ann_data) %>%
        mutate(Value = Volume_m3)
    } else if (input$ann_datatype == 3) {
      ann_data <- add_daily_yield(ann_data, basin_area = input$basinarea) %>%
        mutate(Value = Yield_mm)
    }

    calc_annual_stats(data = ann_data,
                      percentiles = as.numeric(input$ann_ptiles),
                      roll_days = input$ann_roll_days,
                      roll_align = input$ann_roll_align,
                      water_year_start = as.numeric(input$year_start),
                      start_year = input$years_range[1],
                      end_year = input$years_range[2],
                      exclude_years = as.numeric(input$years_exclude),
                      months = as.numeric(input$annual_months),
                      ignore_missing = input$ign_missing_box)
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
      { if(input$ann_datatype == 2) ylab("Volumetric Discharge (m3)") } +
      { if(input$ann_datatype == 3) ylab("Runoff Yield (mm)") } +
      xlab("Year") +
      scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::labs(color = 'Annual Statistics') +
      {if(input$ann_logQ)scale_y_log10(expand = c(0, 0))}+
      {if(input$ann_logQ)annotation_logticks(base= 10,"left", size=0.6,short = unit(.14, "cm"), mid = unit(.3, "cm"), long = unit(.5, "cm"))}+
      {if(!is.null(input$ann_plot_title)) ggtitle(paste(input$ann_plot_title))} +
      ggplot2::theme(legend.position = "right",
                     legend.spacing = ggplot2::unit(0, "cm"),
                     legend.justification = "top",
                     legend.text = ggplot2::element_text(size = 12),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 13),
                     axis.text = ggplot2::element_text(size = 12))
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
                                  basin_area = input$basinarea,
                                  water_year_start = as.numeric(input$year_start),
                                  start_year = input$years_range[1],
                                  end_year = input$years_range[2],
                                  exclude_years = as.numeric(input$years_exclude),
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
                 paste0("water_year = ", ifelse(input$year_start != "1", "TRUE", "FALSE")),
                 paste0("water_year_start = ", as.numeric(input$year_start)),
                 paste0("start_year = ", input$years_range[1]),
                 paste0("end_year = ", input$years_range[2]),
                 paste0("exclude_years = ", ifelse(length(input$years_exclude) == 0, "NULL", list(as.numeric(input$years_exclude)))),
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
    #        "water_year_start = ", as.numeric(input$year_start), ", ",
    #        "start_year = ", input$years_range[1], ", ",
    #        "end_year = ", input$years_range[2], ", ",
    #        "exclude_years = ", as.numeric(input$years_exclude), ", ",
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
                 paste0("water_year = ", ifelse(input$year_start != "1", "TRUE", "FALSE")),
                 paste0("water_year_start = ", as.numeric(input$year_start)),
                 paste0("start_year = ", input$years_range[1]),
                 paste0("end_year = ", input$years_range[2]),
                 paste0("exclude_years = ", ifelse(length(input$years_exclude) == 0, "NULL", list(as.numeric(input$years_exclude)))),
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
                                       water_year_start = as.numeric(input$year_start),
                                       start_year = input$years_range[1],
                                       end_year = input$years_range[2],
                                       exclude_years = as.numeric(input$years_exclude),
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


  output$hydat_stations_table <- renderDataTable(
    stations,
    rownames = FALSE,
    selection = list(mode = "single"),
    filter = 'top',
    extensions = c("Scroller","ColReorder","Buttons"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip',
                   colReorder = TRUE,
                   buttons= list(list(extend = 'colvis', columns = c(1:10))))
  )


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
      ggtitle(paste0("Daily Stream Discharge - ",input$station_name," (",input$yearRange[1],"-",input$yearRange[2],")"))
    print(daily.plot)
  }

  #structure to show the plot interactively
  output$dailyPlot <- renderPlot({
    dailyPlot()
  })

  output$downloadDailyPlot <- downloadHandler(
    filename = function() {paste0(input$station_name," - Daily Discharge Summary"," (",input$yearRange[1],"-",input$yearRange[2],").png")},
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
    filename = function() {paste0(input$station_name," - Daily Discharge Summary"," (",input$yearRange[1],"-",input$yearRange[2],").csv")},
    content = function(file) {
      write.csv(dailyTable(),file, row.names = FALSE)
    })








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
  #   }
  # )

}
