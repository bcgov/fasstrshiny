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



# Volume Frequency - High/Low ------------------
ui_volume_freq <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("High/Low Volume Frequency Analysis"),
      box(width = 3,

          # Buttons
          bsButton(ns("compute"), "Compute Analysis", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          ui_download(id = ns("plot")),
          hr(class = "narrowHr"),

          # Other
          uiOutput(ns("ui_exclude")),

          h3("Options"),
          show_ui(ns("show_data"), "Data"),
          div(id = ns("data"),
              select_rolling(id, multiple = TRUE),
              select_analysis_data(id),
              select_allowed(id)
          ),
          show_ui(ns("show_plotting"), "Probability Plotting"),
          select_analysis_plots(id),

          show_ui(ns("show_fitting"), "Distribution Fitting"),
          select_fitting(id)
      ),

      tabBox(
        width = 9,

        # Plot ---------------------
        tabPanel(
          title = "Plot",
          select_plot_options(select_plot_title(id)),
          ui_plot_selection(id),
          shinycssloaders::withSpinner(ggiraph::girafeOutput(ns("plot")))
        ),

        # Table - Plot Data ---------------------
        tabPanel(
          title = "Table - Plot Data",
          h4(textOutput(ns("table_plot_title"))),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table_plot")))
        ),

        # Table - Fitted Quantiles ---------------------
        tabPanel(
          title = "Table - Fitted Quantiles",
          h4(textOutput(ns("table_fit_title"))),
          shinycssloaders::withSpinner(DT::DTOutput(ns("table_fit")))
        ),

        # Fit Plot ---------------------
        tabPanel(
          title = "Fit Checks",
          uiOutput(ns("ui_day")),
          verbatimTextOutput(ns("fit_stats")),
          shinycssloaders::withSpinner(
            plotOutput(ns("fit_plot"), height = "550px"))
        ),

        # Info ---------------------
        tabPanel(
          title = "Analysis Info"
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_volume_freq <- function(id, data_settings, data_raw,
                               data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # UI Elements ---------------------------------
    # Excluded years, takes defaults from data_settings()$years_exclude,
    # but allowed to modify here
    output$ui_exclude <- renderUI({
      req(data_settings()$years_range)
      selectizeInput(NS(id, "years_exclude"),
                     label = "Years to exclude",
                     choices = seq(from = data_settings()$years_range[1],
                                   to = data_settings()$years_range[2], by = 1),
                     selected = data_settings()$years_exclude,
                     multiple = TRUE)
    })

    # Update years_exclude as points selected/unselected
    observe({
      updateNumericInput(inputId = "years_exclude",
                         value = c(excluded(), input$plot_selected))
    })  %>%
      bindEvent(input$plot_selected, ignoreNULL = FALSE)

    output$ui_day <- renderUI({
      radioGroupButtons(NS(id, "day"),
                        choices = names(freqs()$Freq_Fitting),
                        selected = names(freqs()$Freq_Fitting)[1])
    })

    # Preserve dynamic UI inputs during bookmarking
    setBookmarkExclude(c("compute")) # Set inputs, but user must click button
    keep <- c("years_exclude")
    onBookmark(function(state) for(k in keep) state$values[[k]] <- input[[k]])
    onRestored(function(state) restore_inputs(session, keep, state$values))


    # Toggles
    observe(shinyjs::toggle("data", condition = input$show_data))
    observe(shinyjs::toggle("plotting", condition = input$show_plotting))
    observe(shinyjs::toggle("fitting", condition = input$show_fitting))

    # Change button status -----------------------

    # Current settings
    settings_current <- reactive({
      s <- get_inputs(input, which = c(
        "years_exclude",
        "roll_days", "roll_align", "use_max", "use_log", "allowed",
        "prob_plot", "prob_scale", "plot_curve",
        "fit_quantiles", "fit_distr", "fit_distr_method"))
      s$data_raw <- data_raw()
      s$data_settings <- data_settings()
      s
    })

    # Settings at last Compute
    settings_last <- reactive(settings_current()) %>% bindEvent(input$compute)

    observe({
      settings_current()
      # Change buttons and record status if changes
      if(input$compute > 0) {
        update_on_change(session, id,
                         current = settings_current(), last = settings_last(),
                         labels = paste0("Compute Analysis<br><small>",
                                         c("Settings/Data have changed",
                                           "No changes since last computation"),
                                         "</small>"))
      }
    })



    # Excluded ----------------------------
    # What years were excluded when the trends were last calculated?
    excluded <- reactive({
      input$years_exclude
    }) %>%
      bindEvent(input$compute)

    # Frequencies -----------------------
    freqs <- reactive({

      validate(need(all(!is.na(text_to_num(input$prob_scale))),
                    "Probabilies to plot must be a comma separated list of numbers"))

      data_flow <- data_raw()

      r <- create_fun(fun = "compute_annual_frequencies", data_name = "data_flow",
                      input, input_data = data_settings())

      code$data <- r
      labels$data <- paste0("Compute Annual flow volume frequency analysis ",
                            "(creates all outputs as a list)")
      eval_check(r)
    }) %>%
      bindEvent(input$compute)


    # Titles ----------
    titles <- reactive(title(data_settings(), glue::glue("Volume Frequency")))

    # Plot --------------------
    plot <- reactive({

      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      fit <- freqs()[["Freq_Fitted_Quantiles"]] %>%
        tidyr::pivot_longer(dplyr::matches("^[0-9]+"),
                            names_to = "Measure", values_to = "Quantile") %>%
        dplyr::group_by(.data$Measure) %>%
        dplyr::mutate(
          prob1 = .data$Probability,
          prob2 = dplyr::lead(.data$Probability),
          quant1 = .data$Quantile,
          quant2 = dplyr::lead(.data$Quantile),
          Quantile = round(.data$Quantile, 4),
          `Return Period` = round(.data[["Return Period"]], 4),
          tooltip = glue::glue("Curve: {.data$Measure}<br>",
                               "Probability: {.data$Probability}<br>",
                               "Quantile: {.data$Quantile}<br>",
                               "Return Period: {.data$`Return Period`}"))


      g <- freqs()[["Freq_Plot"]] +
        ggiraph::geom_point_interactive(ggplot2::aes(
          tooltip = glue::glue("Year: {.data$Year}\n",
                               "Discharge: {round(.data$Value, 4)}\n",
                               "Probability: {round(.data$prob, 4)}"),
          data_id = .data$Year), size = 2) +
        ggplot2::scale_colour_viridis_d(end = 0.8) +
        ggiraph::geom_segment_interactive(
          data = fit, ggplot2::aes(x = .data$prob1, xend = .data$prob2,
                                   y = .data$quant1, yend = .data$quant2,
                                   group = .data$Measure,
                                   tooltip = .data$tooltip),
          size = 2, alpha = 0.01)

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(titles()) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }

      g
    })

    dims <- c(10, 6) * opts$scale
    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(),
                      width_svg = dims[1],
                      height_svg = dims[2],
                      options = ggiraph_opts(selection = "multiple"))
    })

    # Remove selected points if changing the numericInput
    observe({
      yrs <- input$years_exclude       # All excluded years
      yrs <- yrs[!yrs %in% excluded()] # Not ones excluded in last run (point doesn't exist)

      if(length(yrs) == 0) yrs <- NULL
      if(!identical(yrs, input$plot_selected)) { # Don't change if no change to make
        if(is.null(yrs)) yrs <- ""
        session$sendCustomMessage(type = 'plot_set', message = yrs)
      }
    }) %>%
      bindEvent(input$years_exclude, ignoreNULL = FALSE, ignoreInit = TRUE)


    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "volume_freq",
             data_settings, dims)




    # Table - Plot data -----------------------
    output$table_plot <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      prep_DT(freqs()[["Freq_Plot_Data"]])
    })
    output$table_plot_title <- renderText(titles())

    # Table - Fitted Quantiles -----------------------
    output$table_fit <- DT::renderDT({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      prep_DT(freqs()[["Freq_Fitted_Quantiles"]])
    })
    output$table_fit_title <- renderText(title(data_settings(), "Fitted Quantiles"))

    # Fit checks --------------------
    output$fit_stats <- renderPrint({
      req(input$day)
      freqs()[["Freq_Fitting"]][[input$day]]
    })

    output$fit_plot <- renderPlot({
      validate(
        need(data_loaded(),
             "You'll need to first load some data under Data > Loading") %then%
          need(input$compute,
               "Choose your settings and click 'Compute Analysis'"))

      req(input$day)
      freqs()[["Freq_Fitting"]][[input$day]] %>%
        gg_fitdistr(title = input$day)
    })





    # Ensure that ui elements are not suspended when hidden
    stop_ui_suspend(id, output)

    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
