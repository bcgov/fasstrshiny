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

# Annual Trends -----------------------
ui_annual_trends <- function(id, plot_height) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Trends"),
      box(width = 3,

          # Compute button
          bsButton(ns("compute"), "Compute Trends", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          hr(class = "narrowHr"),

          # Other options
          uiOutput(ns("ui_exclude")),

          show_ui(ns("show_methods"), "Methods"),
          fluidRow(id = ns("methods"),
                   column(width = 6,
                          awesomeRadio(ns("zyp"),
                                       label = "Prewhitening method",
                                       choices = list("Zhang" = "zhang",
                                                      "Yue-Pilon" = "yuepilon"),
                                       selected = "zhang")),
                   column(width = 6,
                          numericInput(ns("alpha"), label = "Trend alpha",
                                       value = 0.05, min = 0, max = 0.3, step = 0.05)),
                   bsTooltip(ns("zyp"), tips$zyp, placement = "left"),
                   bsTooltip(ns("alpha"), tips$alpha, placement = "left")),
          show_ui(ns("show_options"), "Data Options"),
          div(id = ns("options"),
              fluidRow(
                column(6, selectizeInput(ns("annual_percentiles"),
                                         label = "Annual perc.",
                                         choices = c(1:99),
                                         selected = c(10,90),
                                         multiple = TRUE)),
                column(6, selectizeInput(ns("monthly_percentiles"),
                                         label = "Monthly perc.",
                                         choices = c(1:99),
                                         selected = c(10,20),
                                         multiple = TRUE)),
                bsTooltip(ns("annual_percentiles"), tips$percentiles,
                          placement = "left"),
                bsTooltip(ns("monthly_percentiles"), tips$percentiles,
                          placement = "left")),

              strong("Low Flows"),
              select_rolling(id, name = "low_roll", set = FALSE, multiple = TRUE),

              selectizeInput(ns("percent"),
                             label = "Percents of total annual flows",
                             choices = c(1:99),
                             selected = c(25, 33, 50, 75),
                             multiple = TRUE),
              bsTooltip(ns("percent"), tips$percent, placement = "left"),

              sliderInput(ns("normal"), label = "Days Outside Normal - Range",
                          value = c(25, 75), min = 1, max = 99, step = 1),
              bsTooltip(ns("normal"), tips$normal, placement = "left")
          ),
          show_ui(ns("show_allowed"), "Missing Dates"),
          div(id = ns("allowed"), uiOutput(ns("ui_allowed")))
      ),

      tabBox(
        width = 9, height = 900,

        ### Plot/Table ---------------------
        tabPanel(
          title = "Exploring Trends",
          withSpinner(DT::DTOutput(ns("table_fit"))),
          p(style = "margin-bottom:30px"), # A bit of space

          conditionalPanel(
            "output.plot", ns = NS(id),
            helpText("Click on a point or 'lasso' a bunch to add year to ",
                     "'Years to exclude'. Remember to re-Compute ",
                     "Trends.")),
          ggiraph::girafeOutput(ns("plot"), height = "450px")),

        ### Table ---------------------
        tabPanel(
          title = "Table - Annual Values",
          withSpinner(DT::DTOutput(ns("table_years")))
        ),

        ### Info ---------------------
        tabPanel(
          title = "Analysis Info"
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_annual_trends <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {


    # Excluded years, takes defaults from data_settings$years_exclude,
    # but allowed to modify here
    output$ui_exclude <- renderUI({
      req(data_settings$years_range)
      tagList(
        selectizeInput(NS(id, "years_exclude"),
                       label = "Years to exclude",
                       choices = seq(from = data_settings$years_range[1],
                                     to = data_settings$years_range[2], by = 1),
                       selected = data_settings$years_exclude,
                       multiple = TRUE),
        bsTooltip(id = "years_exclude", title = tips$years_exclude,
                  placement = "left"))
    })

    # Update years_exclude as points selected/unselected
    observe({
      updateNumericInput(inputId = "years_exclude",
                         value = c(excluded(), input$plot_selected))
    }) %>%
      bindEvent(input$plot_selected)

    output$ui_allowed <- renderUI({
      tagList(
        sliderInput(NS(id, "allowed_annual"),
                    label = "Annual - Allowed missing (%)",
                    value = data_settings$allowed, step = 5, min = 0, max = 100),
        sliderInput(NS(id, "allowed_monthly"),
                    label = "Monthly - Allowed missing (%)",
                    value = data_settings$allowed, step = 5, min = 0, max = 100),
        bsTooltip(NS(id, "allowed_annual"), tips$allowed, placement = "left"),
        bsTooltip(NS(id, "allowed_monthly"), tips$allowed, placement = "left")
      )
    })

    # General toggles
    observe(toggle("methods", condition = input$show_methods))
    observe(toggle("options", condition = input$show_options))
    observe(toggle("allowed", condition = input$show_allowed))


    ## Excluded ----------------------------
    # What years were excluded when the trends were last calculated?
    excluded <- reactive({
      input$years_exclude
    }) %>%
      bindEvent(input$compute)

    ## Trends -----------------------
    trends <- reactive({
      req(input$zyp)

      data_flow <- data_raw()

      # ignore_missing / allowed missing
      #basin area?

      # Define parameters
      p <- c(
        glue::glue("exclude_years = c({glue::glue_collapse(input$years_exclude, sep = ', ')})"),
        glue::glue("zyp_method = '{input$zyp}'"),
        glue::glue("annual_percentiles = c({glue::glue_collapse(input$annual_percentiles, sep = ', ')})"),
        glue::glue("monthly_percentiles = c({glue::glue_collapse(input$monthly_percentiles, sep = ', ')})"),
        glue::glue("stats_days = {data_settings$roll_days}"),
        glue::glue("stats_align = '{data_settings$roll_align}'"),
        glue::glue("lowflow_days = c({glue::glue_collapse(input$low_roll_days, sep = ', ')})"),
        glue::glue("lowflow_align = '{input$low_roll_align}'"),
        glue::glue("timing_percent = c({glue::glue_collapse(input$percent, sep = ', ')})"),
        glue::glue("normal_percentiles = c({glue::glue_collapse(input$normal, sep = ', ')})"),
        glue::glue("allowed_missing_annual = {input$allowed_annual}"),
        glue::glue("allowed_missing_monthly = {input$allowed_monthly}"),
        glue::glue("zyp_alpha = {input$alpha}")) %>%
        glue::glue_collapse(sep = ", ")


      r <- create_fun(
        fun = "compute_annual_trends", data = "data_flow", input,
        input_data = data_settings, extra = p, params_ignore = "years_exclude")

      code$data <- r

      eval(parse(text = r))
    }) %>%
      bindEvent(input$compute)

    ## Table - Fit -----------------------
    output$table_fit <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Trends'"))

      req(trends())

      isolate({
        s <- input$table_fit_rows_selected
        if(is.null(s)) s <- 1
      })

      trends()[["Annual_Trends_Results"]] %>%
        DT::datatable(
          rownames = FALSE,
          extensions = c("Scroller"),
          options = list(scrollX = TRUE, scrollY = 250, scroller = TRUE,
                         deferRender = TRUE, dom = 'Brtip'),
          selection = list(target = "row", mode = "single", selected = s))
    })

    ## Stat - to plot ---------------------
    stat <- reactive({
      req(input$table_fit_rows_selected)
      trends()[["Annual_Trends_Results"]] %>%
        slice(input$table_fit_rows_selected) %>%
        pull(Statistic) %>%
        as.character()
    })

    ## Plot --------------------
    output$plot <- ggiraph::renderGirafe({

      s <- stat()
      g <- trends()[[s]] +
        ggiraph::geom_point_interactive(ggplot2::aes(
          tooltip = paste0("Year: ", Year, "\n",
                           s, ": ", round(Value, 4)),
          data_id = Year), size = 4, na.rm = TRUE)

      ggiraph::girafe(ggobj = g, width_svg = 7, height_svg = 5,
                      options = list(ggiraph::opts_selection(type = "multiple"),
                                     ggiraph::opts_toolbar(position = "topleft")))
    })

    # Add/Remove selected points if changing the numericInput
    observe({
      yrs <- input$years_exclude       # All excluded years
      yrs <- yrs[!yrs %in% excluded()] # Not ones excluded in last run (point doesn't exist)
      if(length(yrs) == 0) yrs <- NULL

      if(!identical(yrs, input$plot_selected)) {
        if(is.null(yrs)) yrs <- ""
        session$sendCustomMessage(type = 'plot_set', message = yrs)
      }
    }) %>%
      bindEvent(input$years_exclude, ignoreNULL = FALSE, ignoreInit = TRUE)



    ## Table - years sub -----------------------
    output$table_years_sub <- gt::render_gt({

      trends()[[1]] %>%
        filter(Statistic == stat()) %>%
        select(-any_of(c("STATION_NUMBER", "Statistic"))) %>%
        pivot_longer(cols = everything(),
                     names_to = "Year",
                     values_to = stat()) %>%
        gt() %>%
        fmt_number(-Year, decimals = 4)
    }, height = px(400))

    ## Table - years -----------------------
    output$table_years <- DT::renderDT({

      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Trends'"))

      req(trends())

      prep_DT(trends()[[1]])

    })

    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(output)

    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}