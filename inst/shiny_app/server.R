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

  ## Flows ----------------------------------------------------------

  # Plot options
  output$ui_flows_plot_options <- renderUI({
    select_plot_options(
      select_plot_log(id = "flows",
                      value = formals(plot_flow_duration)$log_discharge))
  })


  ## Annual Statistics --------------------------------------------
  # Plot options
  output$ui_as_plot_options <- renderUI({
    id <- "as"
    select_plot_options(
      select_plot_log(id,
                      value = formals(plot_monthly_stats2)$log_discharge))
    # Add inner/outer percentiles?
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
    radioGroupButtons("vf_day",
                      choices = names(vf_freqs()$Freq_Fitting),
                      selected = names(vf_freqs()$Freq_Fitting)[1])
  })


  ## General Toggles ---------------------
  # Hide/Show based on toggle
  observe(toggle("data_stn", condition = input$data_show_stn))
  observe(toggle("data_dates", condition = input$data_show_dates))
  observe(toggle("data_types", condition = input$data_show_types))

  observe(toggle("at_methods", condition = input$at_show_methods))
  observe(toggle("at_options", condition = input$at_show_options))
  observe(toggle("ui_at_allowed", condition = input$at_show_allowed))
  observe(toggle("vf_data", condition = input$vf_show_data))
  observe(toggle("vf_plotting", condition = input$vf_show_plotting))
  observe(toggle("vf_fitting", condition = input$vf_show_fitting))

  # Enable/Disable based on toggle
  observe(toggleState("hydro_add_dates", condition = input$hydro_type == "Daily"))
  observe(toggleState("hydro_custom_months_all", condition = input$hydro_type != "Daily"))
















  # Flows --------------------------------------

  ## Plot --------------------

  output$flows_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())

    data_flow <- data_raw()

    # missing arguments
    # - include_longterm

    g <- create_fun(fun = "plot_flow_duration", data = "data_flow",
                    input, input_data = data_settings)

    code$flows_plot <- g

    g <- eval(parse(text = g))[[1]]
    g <- g +
      ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = glue::glue(
          "Month: {Month}\n",
          "{Percentile}% time\n",
          "Discharge cutoff: {round(Value, 3)}",
          .trim = FALSE),
          data_id = Percentile),
        show.legend = FALSE, alpha = 0.01, size = 3)

    ggiraph::girafe(
      ggobj = g, width_svg = 12, height = 6,
      options = list(
        ggiraph::opts_toolbar(position = "topleft"),
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "fill:orange; stroke:gray;fill-opacity:1;")))
  }) %>%
    bindEvent(input$flows_compute, ignoreNULL = FALSE)


  ## Table -----------------------
  output$flows_table <- DT::renderDT({
    check_data(data_loaded())

    data_flow <- data_raw()

    t <- create_fun(
      fun = "calc_longterm_daily_stats",
      data = "data_flow", input, input_data = data_settings,
      extra = "percentiles = 1:99",
      end = "%>% select(-Mean, -Median, -Minimum, -Maximum)")

    code$flows_table <- t

    parse(text = t) %>%
      eval() %>%
      tidyr::pivot_longer(cols = -c(STATION_NUMBER, Month),
                          names_to = "percentiles", values_to = "value") %>%
      tidyr::pivot_wider(names_from = Month, values_from = value) %>%
      prep_DT()
  }) %>%
    bindEvent(input$flows_compute, ignoreNULL = FALSE)





  ## R Code -----------------
  output$flows_code <- renderText({
    code_format(code, id = "flows")
  })



  # Annual Statistics -----------------

  ## Plot -----------------------------
  output$as_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())

    data_flow <- data_raw()

    g <- switch(input$as_type,
                "Monthly" = "plot_monthly_stats2",
                "Annual" = "plot_annual_stats2") %>%
      ("data_flow", input, input_data = data_settings)

    code$as_plot <- g

    g <- eval(parse(text = g))[[1]]


    # Add interactivity
    date_cols <- "Year"
    if(input$as_type == "Monthly") date_cols <- c(date_cols, "Month")
    stats <- names(g$data) # Get stats from plot data
    #stats <- stats[!stats %in% date_cols] # Omit these

    # Add vline
    g <- g + create_vline_interactive(
      data = g$data, stats, size = if_else(input$as_type == "Annual", 10, 2))

    ggiraph::girafe(
      ggobj = g, width_svg = 12, height = 6,
      options = list(
        ggiraph::opts_toolbar(position = "topleft"),
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
  })

  ## Table -----------------------
  output$as_table <- DT::renderDT({
    check_data(data_loaded())
    req(input$as_type)

    data_flow <- data_raw()

    t <- switch(input$as_type,
                "Monthly" = "calc_monthly_stats",
                "Annual" = "calc_annual_stats") %>%
      create_fun("data_flow", input, input_data = data_settings)

    code$as_table <- t

    parse(text = t) %>%
      eval() %>%
      prep_DT()
  })


  ## R Code -----------------
  output$as_code <- renderText({
    code_format(code, id = "as")
  })



  # Annual Means ---------------------------------------


  ## Plot --------------------
  output$am_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())

    data_flow <- data_raw()

    g <- create_fun(fun = "plot_annual_means", data = "data_flow",
                    input, input_data = data_settings,
                    params_ignore = "discharge")

    code$am_plot <- g

    g <- eval(parse(text = g))[[1]]

    # Replace layers with interactive
    g$layers[[1]] <- ggiraph::geom_bar_interactive(
      ggplot2::aes(tooltip = glue::glue("Year: {Year}\n",
                                        "MAD Diff: {round(MAD_diff, 4)}",
                                        .trim = FALSE),
                   data_id = Year, colour = "Annual MAD difference"),
      fill = "cornflowerblue", stat = "identity")

    g <- g + ggplot2::geom_hline(ggplot2::aes(yintercept = 0,
                                              colour = "Long-term MAD"),
                                 size = 1) +
      ggplot2::scale_colour_manual(
        values = c("Long-term MAD" = "black",
                   "Annual MAD difference" = "cornflowerblue")) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
        fill = c("black", "cornflowerblue")))) +
      ggplot2::theme(legend.position = c(0.8, 0.8),
                     legend.title = element_blank(),
                     legend.key = element_rect(colour = NA))
    ggiraph::girafe(ggobj = g, width_svg = 14, height_svg = 4,
                    options = list(ggiraph::opts_selection(type = "none")))
  })


  ## R Code -----------------
  output$am_code <- renderText({
    code_format(code, id = "am")
  })

  # Annual Totals ----------------------------------

  ## Plot --------------------
  output$ann_tot_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())
    req(!is.null(input$ann_tot_seasons))

    data_flow <- data_raw()

    g <- create_fun(fun = "plot_annual_cumulative_stats", data = "data_flow",
                    input, input_data = data_settings,
                    extra = glue::glueglue("use_yield = {input$ann_tot_discharge}, ",
                                 "include_seasons = {input$ann_tot_seasons}"))

    code$ann_tot_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))


    # Add individual geoms to each plot (annual has more than one)
    for(i in seq_along(g)) {
      g[[i]] <- g[[i]] + ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                      Statistic, ": ", round(Value, 4)),
                     data_id = Year), size = 3)
    }

    g <- g %>%
      wrap_plots(nrow = 2, byrow = FALSE, design = "AC
                                                    BC")

    ggiraph::girafe(
      ggobj = g, width_svg = 14, height_svg = 6,
      options = list(
        ggiraph::opts_toolbar(position = "topleft"),
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(
          css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
  })


  ## Table -----------------------
  output$ann_tot_table <- DT::renderDT({
    check_data(data_loaded())

    data_flow <- data_raw()

    t <- create_fun("calc_annual_cumulative_stats", data = "data_flow",
                    input, input_data = data_settings,
                    extra = glue::glueglue("use_yield = {input$ann_tot_discharge}",
                                 "include_seasons = {input$ann_tot_seasons}"))

    code$ann_tot_table <- t

    parse(text = t) %>%
      eval() %>%
      prep_DT()
  })


  ## R Code -----------------
  output$ann_tot_code <- renderText({
    code_format(code, id = "")
  })




  # Flow timing ---------------------------------------
  ## Plot --------------------
  output$ft_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())
    req(input$ft_percent)

    data_flow <- data_raw()

    g <- create_fun(
      fun = "plot_annual_flow_timing", data = "data_flow",
      input, input_data = data_settings,
      extra = glue::glueglue("percent_total = ",
                   "c({glue::glueglue_collapse(input$ft_percent, sep = ',')})"))

    code$ft_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))[[1]]

    g <- g +
      ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                      Statistic, "\n",
                                      "Day of Year: ", Value),
                     data_id = Year), size = 3)

    ggiraph::girafe(ggobj = g, width_svg = 14, height_svg = 8,
                    options = list(
                      ggiraph::opts_toolbar(position = "topleft"),
                      ggiraph::opts_selection(type = "none")))
  })


  ## Table -----------------------
  output$ft_table <- DT::renderDT({
    check_data(data_loaded())
    req(input$ft_percent)

    data_flow <- data_raw()

    t <- create_fun(
      fun = "calc_annual_flow_timing", data = "data_flow",
      input, input_data = data_settings,
      extra = glue::glue("percent_total = ",
                   "c({glue::glue_collapse(input$ft_percent, sep = ',')})"))

    code$ft_table <- t

    parse(text = t) %>%
      eval() %>%
      prep_DT()
  })


  ## R Code -----------------
  output$ft_code <- renderText({
    code_format(code, id = "ft")
  })


  # Low Flows ---------------------------------------
  ## Plot --------------------
  output$lf_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())

    data_flow <- data_raw()

    g <- create_fun(fun = "plot_annual_lowflows", data = "data_flow",
                    input, input_data = data_settings)

    code$lf_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))

    g[["Annual_Low_Flows"]] <- g[["Annual_Low_Flows"]] +
      ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                      Statistic, "\n",
                                      "Discharge: ", round(Value, 4)),
                     data_id = Year), size = 3)

    g[["Annual_Low_Flows_Dates"]] <- g[["Annual_Low_Flows_Dates"]] +
      ggiraph::geom_point_interactive(
        ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                      Statistic, "\n",
                                      "Day of Year: ", round(Value, 4)),
                     data_id = Year), size = 3)

    # Combine plots
    g <- wrap_plots(g)

    ggiraph::girafe(ggobj = g, width_svg = 12, height_svg = 8,
                    options = list(
                      ggiraph::opts_toolbar(position = "topleft"),
                      ggiraph::opts_selection(type = "none")))
  })


  ## Table -----------------------
  output$lf_table <- DT::renderDT({
    check_data(data_loaded())

    data_flow <- data_raw()

    t <- create_fun(fun = "calc_annual_lowflows", data = "data_flow",
                    input, input_data = data_settings)

    code$lf_table <- t

    parse(text = t) %>%
      eval() %>%
      prep_DT()
  })


  ## R Code -----------------
  output$lf_code <- renderText({
    code_format(code, id = "lf")
  })

  # Peak Flows ---------------------------------------

  ## Table -----------------------
  output$pf_table <- DT::renderDT({
    check_data(data_loaded())

    data_flow <- data_raw()

    t <- create_fun(fun = "calc_annual_peaks", data = "data_flow",
                    input, input_data = data_settings)

    code$pf_table <- t

    parse(text = t) %>%
      eval() %>%
      prep_DT()
  })


  ## R Code -----------------
  output$pf_code <- renderText({
    code_format(code, id = "pf")
  })

  # Days outside normal ---------------------------------------
  ## Plot --------------------
  output$on_plot <- ggiraph::renderGirafe({
    check_data(data_loaded())
    req(input$on_normal)

    data_flow <- data_raw()

    g <- create_fun(
      fun = "plot_annual_outside_normal", data = "data_flow",
      input, input_data = data_settings,
      extra = glue::glue("normal_percentiles = ",
                   "c({glue::glue_collapse(input$on_normal, sep = ',')})"))

    code$on_plot <- g

    # Add interactivity
    g <- eval(parse(text = g))[[1]]

    g <- g + ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                    Statistic, "\n",
                                    "No. Days: ", round(Value, 4)),
                   data_id = Year), size = 3)

    ggiraph::girafe(
      ggobj = g, width_svg = 12, height_svg = 8,
      options = list(
        ggiraph::opts_toolbar(position = "topleft"),
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
  })


  ## Table -----------------------
  output$on_table <- DT::renderDT({
    req(input$on_normal)

    data_flow <- data_raw()

    t <- create_fun(
      fun = "calc_annual_outside_normal", data = "data_flow",
      input, input_data = data_settings,
      extra = glue::glue("normal_percentiles = ",
                   "c({glue::glue_collapse(input$on_normal, sep = ',')})"))

    code$on_table <- t

    parse(text = t) %>%
      eval() %>%
      prep_DT()
  })


  ## R Code -----------------
  output$on_code <- renderText({
    code_format(code, id = "on")
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
    req(input$at_zyp)

    data_flow <- data_raw()

    # ingore_missing / allowed missing
    #basin area?

    # Define parameters
    p <- c(
      glue::glue("exclude_years = c({glue::glue_collapse(input$at_years_exclude, sep = ', ')})"),
      glue::glue("zyp_method = '{input$at_zyp}'"),
      glue::glue("annual_percentiles = c({glue::glue_collapse(input$at_annual_percentiles, sep = ', ')})"),
      glue::glue("monthly_percentiles = c({glue::glue_collapse(input$at_monthly_percentiles, sep = ', ')})"),
      glue::glue("stats_days = {input$data_roll_days}"),
      glue::glue("stats_align = '{input$data_roll_align}'"),
      glue::glue("lowflow_days = c({glue::glue_collapse(input$at_low_roll_days, sep = ', ')})"),
      glue::glue("lowflow_align = '{input$at_low_roll_align}'"),
      glue::glue("timing_percent = c({glue::glue_collapse(input$at_percent, sep = ', ')})"),
      glue::glue("normal_percentiles = c({glue::glue_collapse(input$at_normal, sep = ', ')})"),
      glue::glue("allowed_missing_annual = {input$at_allowed_annual}"),
      glue::glue("allowed_missing_monthly = {input$at_allowed_monthly}"),
      glue::glue("zyp_alpha = {input$at_alpha}")) %>%
      glue::glue_collapse(sep = ", ")


    r <- create_fun(
      fun = "compute_annual_trends", data = "data_flow", input,
      input_data = data_settings, extra = p, params_ignore = "years_exclude")

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
      prep_DT()
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
  output$at_plot <- ggiraph::renderGirafe({

    g <- at_trends()[[at_stat()]] +
      ggiraph::geom_point_interactive(aes(
        tooltip = paste0("Year: ", Year, "\n",
                         at_stat(), ": ", round(Value, 4)),
        data_id = Year), size = 4)

    ggiraph::girafe(ggobj = g, width_svg = 7, height_svg = 5,
                    options = list(ggiraph::opts_selection(type = "multiple"),
                                   ggiraph::opts_toolbar(position = "topleft")))
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

    prep_DT(at_trends()[[1]])

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

    validate(need(all(!is.na(text_to_num(input$vf_prob_scale))),
                  "Probabilies to plot must be a comma separated list of numbers"))

    data_flow <- data_raw()

    # Define parameters
    p <- c(
      glue::glue("exclude_years = c({glue::glue_collapse(input$vf_years_exclude, sep = ', ')})"),
      glue::glue("use_max = {input$vf_use_max}"),
      glue::glue("use_log = {input$vf_log}"),
      glue::glue("roll_days = c({glue::glue_collapse(input$vf_roll_days, sep = ', ')})"),
      glue::glue("roll_align = '{input$vf_roll_align}'"),
      glue::glue("prob_plot_position = '{input$vf_prob_plot}'"),
      glue::glue("prob_scale_points = c({input$vf_prob_scale})"),
      glue::glue("fit_distr = '{input$vf_fit_distr}'"),
      glue::glue("fit_distr_method = '{input$vf_fit_distr_method}'"),
      glue::glue("fit_quantiles = c({glue::glue_collapse(input$vf_fit_quantiles, sep = ', ')})"),
      glue::glue("plot_curve = {input$vf_plot_curve}")) %>%
      glue::glue_collapse(sep = ", ")

    r <- create_fun(fun = "compute_annual_frequencies", data = "data_flow",
                    input, input_data = data_settings,
                    extra = p, params_ignore = "years_exclude")

    code$vf_data <- r

    eval(parse(text = r))
  }) %>%
    bindEvent(input$vf_compute)

  ## Plot --------------------
  output$vf_plot <- ggiraph::renderGirafe({

    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$vf_compute,
             "Choose your settings and click 'Compute Analysis'"))

    g <- vf_freqs()[["Freq_Plot"]] +
      ggiraph::geom_point_interactive(ggplot2::aes(
        tooltip = paste0("Year: ", Year, "\n",
                         "Discharge", ": ", round(Value, 4), "\n",
                         "Probability", ": ", round(prob, 4)),
        data_id = Year), size = 3) +
      ggplot2::scale_colour_viridis_d(end = 0.8)

    ggiraph::girafe(ggobj = g, width_svg = 8, height_svg = 5,
                    options = list(ggiraph::opts_selection(type = "multiple"),
                                   ggiraph::opts_toolbar(position = "topleft")))
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

    prep_DT(vf_freqs()[["Freq_Plot_Data"]])
  })

  ## Table - Fitted Quantiles -----------------------
  output$vf_table_fit <- DT::renderDT({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$vf_compute,
             "Choose your settings and click 'Compute Analysis'"))

    prep_DT(vf_freqs()[["Freq_Fitted_Quantiles"]])
  })

  ## Fit checks --------------------
  output$vf_fit_stats <- renderPrint({
    req(input$vf_day)
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
      gg_fitdistr(title = input$vf_day)
  })



  ## R Code -----------------
  output$vf_code <- renderText({
    code_format(code, id = "vf")
  })



  # Volume Frequency - HYDAT Peak -------------------------------------------

  ## Frequencies -----------------------
  hp_freqs <- reactive({

    validate(need(
      isTruthy(data_raw()$STATION_NUMBER) &
        length(unique(data_raw()$STATION_NUMBER)) == 1,
      paste0("This analysis is only available for HYDAT data with a ",
             "valid STATION_NUMBER")))

    validate(need(all(!is.na(text_to_num(input$hp_prob_scale))),
                  "Probabilies to plot must be a comma separated list of numbers"))

    data_flow <- data_raw()

    # Define parameters
    p <- c(
      glue::glue("station_number = '{unique(data_flow$STATION_NUMBER)}'"),
      glue::glue("use_max = {input$hp_use_max}"),
      glue::glue("use_log = {input$hp_log}"),
      glue::glue("prob_plot_position = '{input$hp_prob_plot}'"),
      glue::glue("prob_scale_points = c({input$hp_prob_scale})"),
      glue::glue("fit_distr = '{input$hp_fit_distr}'"),
      glue::glue("fit_quantiles = c({glue::glue_collapse(input$hp_fit_quantiles, sep = ', ')})"),
      glue::glue("plot_curve = {input$vf_plot_curve}")) %>%
      glue::glue_collapse(sep = ", ")

    r <- create_fun(fun = "compute_hydat_peak_frequencies", input = input,
                    input_data = data_settings, extra = p)

    code$hp_data <- r

    eval(parse(text = r))
  }) %>%
    bindEvent(input$hp_compute)

  ## Plot --------------------
  output$hp_plot <- ggiraph::renderGirafe({

    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$hp_compute,
             "Choose your settings and click 'Compute Analysis'"))

    # Add interactivity
    g <- hp_freqs()[["Freq_Plot"]] +
      ggiraph::geom_point_interactive(ggplot2::aes(
        tooltip = paste0("Year: ", Year, "\n",
                         "Probabily: ", round(prob, 4), "\n",
                         "Discharge: ", round(Value, 4), "\n",
                         "Return Period: ", round(Return_P)),
        data_id = Year), size = 4)

    ggiraph::girafe(ggobj = g,
                    options = list(ggiraph::opts_selection(type = "none"),
                                   ggiraph::opts_toolbar(position = "topleft")))
  })


  ## Table -----------------------
  output$hp_table <- DT::renderDT({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$hp_compute,
             "Choose your settings and click 'Compute Analysis'"))

    prep_DT(hp_freqs()[["Freq_Fitted_Quantiles"]])
  })

  ## Fit checks --------------------
  output$hp_fit_stats <- renderPrint({
    hp_freqs()[["Freq_Fitting"]][[1]]
  })

  output$hp_fit_plot <- renderPlot({
    validate(
      need(input$data_load,
           "You'll need to first load some data under Data > Loading") %then%
        need(input$hp_compute,
             "Choose your settings and click 'Compute Analysis'"))
    hp_freqs()[["Freq_Fitting"]][[11]] %>%
      gg_fitdistr(title = "")
  })



  ## R Code -----------------
  output$hp_code <- renderText({
    code_format(code, id = "hp")
  })

}





