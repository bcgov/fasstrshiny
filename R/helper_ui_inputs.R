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

# Functions --------------------------------------------

# The following functions define inputs including tooltips
# Some are 'fancy' from the shinyWidgets package (e.g., materialSwitch(), etc), and require an extra id to be attached to the label in order to properly display tooltips (see https://github.com/dreamRs/shinyWidgets/issues/63)


## Functions for inputs ------------
select_custom <- function(id, values) {
  div(id = NS(id, "custom_all"),
      prettySwitch(NS(id, "add_custom"),
                   label = "Add custom lines marking discharge",
                   value = FALSE,
                   status = "success", slim = TRUE),
      column(width = 6,
             numericInput(NS(id, "custom"),
                          label = "Discharge:",
                          min = min(values, na.rm = TRUE),
                          max = max(values, na.rm = TRUE),
                          value = round(stats::median(values, na.rm = TRUE), 2),
                          step = 0.01)),
      column(width = 6,
             textInput(NS(id, "custom_label"),
                       label = "Label for line:",
                       placeholder = "ex. Threshold",
                       value = "My Label")),
      bsTooltip(id = NS(id, "custom_all"),
                title = glue::glue(
                  "Add special lines marking discharge?<br>",
                  "Discharge value at which to add a line<br>",
                  "Label for the line on the plot"),
                placement = "left")
  )
}

select_custom_months <- function(id) {
  fluidRow(id = NS(id, "custom_months_all"),
           column(width = 6,
                  selectizeInput(NS(id, "custom_months"),
                                 label = "Months:",
                                 choices = list("Jan" = 1,  "Feb" = 2,
                                                "Mar" = 3,  "Apr" = 4,
                                                "May" = 5,  "Jun" = 6,
                                                "Jul" = 7,  "Aug" = 8,
                                                "Sep" = 9,  "Oct" = 10,
                                                "Nov" = 11, "Dec" = 12),
                                 selected = NULL,
                                 multiple = TRUE)),
           column(width = 6,
                  textInput(NS(id, "custom_months_label"),
                            label = "Label:",
                            placeholder = "ex. Jun-Aug",
                            value = "")),
           bsTooltip(id = NS(id, "custom_months_all"),
                     title = glue::glue("Months: {tips$custom_months}<br>",
                                        "Label: {tips$custom_months_label}"),
                     placement = "left")
  )
}

select_discharge <- function(id) {
  tagList(
    awesomeRadio(NS(id, "discharge"),
                 label = "Discharge Unit Type:",
                 choices = list("Discharge (cms)" = "Value",
                                "Volumetric Discharge (m3)" = "Volume_m3",
                                "Runoff Yield (mm)" = "Yield_mm"),
                 selected = "Value"),
    bsTooltip(NS(id, "discharge"), tips$discharge,
              placement = "left"))
}

select_rolling <- function(id, name = "roll", multiple = FALSE) {
  if(multiple) d <- c(1, 3, 7, 30) else d <- 1

  tagList(
    fluidRow(id = NS(id, glue::glue("{name}ing")),
             column(6,
                    selectizeInput(NS(id, glue::glue("{name}_days")),
                                   label = "Rolling Average Days:",
                                   choices = 1:180,
                                   selected = d,
                                   multiple = multiple)),
             column(6,
                    selectizeInput(NS(id, glue::glue("{name}_align")),
                                   label = "Rolling Average Alignment:",
                                   selected = "right",
                                   choices = list("Right" = "right",
                                                  "Left" = "left",
                                                  "Center" = "center")))
    ),
    bsTooltip(
      id = NS(id, glue::glue("{name}ing")),
      title = glue::glue("Days: {tips$roll_days}<br>Align: {tips$roll_align}"),
      placement = "left"))
}


select_percentiles <- function(id, name = "percentiles", selected = c(10, 90),
                               label = "Percentiles to calculate:", maxItems = 100) {

  if(!is.null(tips[[name]])) t <- tips[[name]] else t <- tips[["percentiles"]]

  tagList(
    selectizeInput(NS(id, name),
                   label = label,
                   choices = c(0:100),
                   selected = selected,
                   multiple = TRUE,
                   options = list(maxItems = maxItems)),
    bsTooltip(NS(id, name), t, placement = "left"))
}

select_percentiles_slide <- function(id, name = "percentiles", value = c(10, 90),
                                     label = "Percentiles to calculate:") {

  if(!is.null(tips[[name]])) t <- tips[[name]] else t <- tips[["percentiles"]]

  tagList(
    sliderInput(NS(id, name),
                label = label,
                value = value,
                min = 0, max = 100, step = 1),
    bsTooltip(NS(id, name), t, placement = "left"))

}

select_complete <- function(id) {

  tagList(
    div(id = NS(id, "complete_tip"),
        prettySwitch(NS(id, "complete"),
                     label = "Complete Years Only",
                     value = FALSE,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "complete_tip"), tips$complete,
              placement = "left"))
}

select_missing <- function(id) {
  tagList(
    div(id = NS(id, "missing_tip"),
        prettySwitch(NS(id, "missing"),
                     value = FALSE, status = "danger",
                     label = "Ignore Missing Values",
                     slim = TRUE)),
    bsTooltip(NS(id, "missing_tip"), tips$missing,
              placement = "left"))
}

select_allowed <- function(id) {
  tagList(
    sliderInput(NS(id, "allowed"),
                label = "Allowed Missing (%)",
                value = 0, step = 5, min = 0, max = 100),
    bsTooltip(NS(id, "allowed"), tips$allowed,
              placement = "left"))
}

select_plot_stats <- function(id, stats) {
  if(!is.null(stats)) {
    tagList(
      radioGroupButtons(NS(id, "stats"),
                        label = "Statistics:",
                        choices = stats,
                        selected = stats),
      bsTooltip(NS(id, "stats"), tips$stats,
                placement = "left"))
  }
}

select_plot_log <- function(id, value = TRUE, name = "plot_log") {

  tagList(
    div(id = NS(id, glue::glue("{name}_tip")),
        prettySwitch(NS(id, name),
                     label = "Use Log Scale",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, glue::glue("{name}_tip")), tips$plot_log,
              placement = "left"))
}


select_plot_extremes <- function(id, value = TRUE) {

  tagList(
    div(id = NS(id, "plot_extremes_tip"),
        prettySwitch(NS(id, "plot_extremes"),
                     label = "Plot Extreme Values",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "plot_extremes_tip"), tips$plot_extremes,
              placement = "left"))
}
select_plot_inner_percentiles <- function(id, value = TRUE) {
  tagList(
    div(id = NS(id, "plot_inner_percentiles_tip"),
        prettySwitch(NS(id, "plot_inner_percentiles"),
                     label = "Plot Inner Percentiles",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "plot_inner_percentiles_tip"), tips$plot_inner_percentiles,
              placement = "left"))
}
select_plot_outer_percentiles <- function(id, value = TRUE) {
  tagList(
    div(id = NS(id, "plot_outer_percentiles_tip"),
        prettySwitch(NS(id, "plot_outer_percentiles"),
                     label = "Plot Outer Percentiles",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "plot_outer_percentiles_tip"), tips$plot_outer_percentiles,
              placement = "left"))
}


select_daterange <- function(id, data) {

  if(is.null(data)) stop("Require 'data' to create daterange UI",
                         call. = FALSE)
  dateRangeInput(NS(id, "daterange"),
                 label = "Start/End dates of data to plot:",
                 format = "yyyy-mm-dd", startview = "month",
                 start = min(data$Date), end = max(data$Date))
  # Tooltip didn't work? (even with tags$span trick)
}

select_add_year <- function(id, years_range) {
  tagList(
    selectizeInput(NS(id, "add_year"),
                   label = "Year to Add:",
                   choices = c("Choose a year" = "",
                               seq(from = years_range[1],
                                   to = years_range[2], by = 1)),
                   selected = NULL,
                   multiple = FALSE),
    bsTooltip(NS(id, "add_year"), tips$add_year,
              placement = "left")
  )
}

select_year_to_plot <- function(id, years_range) {
  tagList(
    selectizeInput(NS(id, "year_to_plot"),
                   label = "Year to Plot:",
                   choices = c(years_range),
                   selected = years_range[1])
  )
}


select_add_dates <- function(id) {
  d <- stats::setNames(1:365, format(as.Date(1:365, origin = "1899-12-31"),
                                     "%b-%d"))
  tagList(
    selectizeInput(
      NS(id, "add_dates"),
      label = "Date to Show:",
      choices = c("Choose date(s)" = "", d),
      selected = NULL, multiple = TRUE),
    bsTooltip(NS(id, "add_dates"), tips$add_dates,
              placement = "left")
  )
}


select_add_mad <- function(id) {
  tagList(
    div(id = NS(id, "add_mad_tip"),
        prettySwitch(NS(id, "add_mad"),
                     label = "Add MAD Values",
                     value = FALSE,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "add_mad_tip"), tips$add_mad,
              placement = "left"))
}

select_plot_title <- function(id, name = "plot_title") {
  div(id = NS(id, glue::glue("{name}_tip")),
      prettySwitch(NS(id, name),
                   label = "Add Plot Title",
                   value = TRUE, status = "success", slim = TRUE),
      bsTooltip(NS(id, glue::glue("{name}_tip")), "Add/remove title from plot",
                placement = "left"))
}

select_plot_display <- function(id, plots, label = "Display Plot") {
  plot_names <- names(plots) %>%
    stats::setNames(., stringr::str_replace_all(., "_", " "))

  tagList(
    awesomeRadio(NS(id, "display"), label,
                 choices = plot_names),
    bsTooltip(NS(id, "display"),
              paste0("Choose plot type to display."),
              placement = "left"))
}

select_fitting <- function(id) {
  tagList(
    div(id = NS(id, "fitting"),

        selectizeInput(
          NS(id, "fit_quantiles"),
          label = "Quantiles to Estimate:",
          choices = seq(0.01, 0.999, 0.0025),
          selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                       0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
          multiple = TRUE),

        column(width = 6, id = NS(id, "fit_distr_tip"),
               awesomeRadio(NS(id, "fit_distr"),
                            label = "Distribution:",
                            choices = list("PIII" = "PIII",
                                           "Weibull" = "weibull")),
        ),
        column(width = 6, id = NS(id, "plot_curve_tip"),
               style = "padding-top: 25px; margin-bottom: 25px",
               prettySwitch(NS(id, "plot_curve"),
                            label = tags$span(strong("Plot Curve")),
                            value = TRUE, status = "success", slim = TRUE)),
        awesomeRadio(
          NS(id, "fit_distr_method"),
          label = "Distribution Method:",
          choices = list("Method of Moments (MOM)" = "MOM",
                         "Maximum Likelihood Estimation (MLE)" = "MLE"))),

    bsTooltip(NS(id, "fit_quantiles"), tips$fit_quantiles,
              placement = "left"),
    bsTooltip(NS(id, "fit_distr_tip"), tips$fit_distr,
              placement = "left"),
    bsTooltip(NS(id, "plot_curve_tip"), tips$plot_curve,
              placement = "left"),
    bsTooltip(NS(id, "fit_distr_method"), tips$fit_distr_method,
              placement = "left")

  )
}

select_analysis_plots <- function(id) {

  div(id = NS(id, "plotting"),
      fluidRow(
        column(6, id = NS(id, "prob_plot_tip"),
               awesomeRadio(NS(id, "prob_plot"),
                            label = "Plotting Positions:",
                            choices = list("Weibull" = "weibull",
                                           "Median" = "median",
                                           "Hazen" = "hazen"))),
        column(
          6, id = NS(id, "prob_scale_tip"),
          textInput(
            NS(id, "prob_scale"),
            label = "Probabilities to Plot:",
            value = paste0("0.9999, 0.999, 0.99, 0.9, 0.5, 0.2, ",
                           "0.1, 0.02, 0.01, 0.001, 0.0001")))
      ),
      bsTooltip(NS(id, "prob_plot_tip"), tips$prob_plot,
                placement = "left"),
      bsTooltip(NS(id, "prob_scale_tip"), tips$prob_scale,
                placement = "left")
  )
}

select_analysis_data <- function(id) {
  fluidRow(
    column(
      width = 6, id = NS(id, "use_max_tip"),
      awesomeRadio(NS(id, "use_max"),
                   label = "Analysis Type:",
                   choices = list("Low Flow" = FALSE,
                                  "High Flow" = TRUE),
                   selected = FALSE, inline = TRUE)),
    column(
      width = 6, id = NS(id, "use_log_tip"), style = "padding-top: 25px",
      prettySwitch(
        NS(id, "use_log"),
        label = tags$span(strong("Log-transform")),
        value = FALSE, status = "success", slim = TRUE)),
    bsTooltip(NS(id, "use_max_tip"), tips$use_max,
              placement = "left"),
    bsTooltip(NS(id, "use_log_tip"), tips$use_log,
              placement = "left")
  )
}



select_plot_options <- function(...) {
  div(
    align = "right",
    dropdownButton(
      tags$h4("Plot Options"),
      tagList(...),
      status = "primary", icon = icon("gear", verify_fa = FALSE),
      size = "sm", width = "300px", right = TRUE,
      tooltip = tooltipOptions(title = "Plot Options", placement = "left")
    )
  )
}

select_table_options <- function(id,
                                 include = c("percentiles", "custom_months"),
                                 params = NULL, data = NULL) {

  i <- tagList()
  if("percentiles" %in% include) i <- tagList(i, select_percentiles(id))
  if("custom_months" %in% include) i <- tagList(i, select_custom_months(id))

  div(
    align = "right",
    dropdownButton(
      tags$h3("Table options"),
      i,
      status = "primary", icon = icon("gear", verify_fa = FALSE),
      size = "sm", width = "300px", right = TRUE,
      tooltip = tooltipOptions(title = "Table options", placement = "left")
    )
  )
}

show_ui <- function(id, name, value = FALSE) {
  h4(materialSwitch(inputId = id,
                    label = name,
                    status = "primary",
                    value = value))
}

