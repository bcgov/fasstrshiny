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


# Determine the default value, if input exists and set is TRUE, returns
# global options preset, otherwise default
set_input <- function(type, input, set, value) {
  if(set & !is.null(input[[glue::glue("data-{type}")]])) {
    value <- input[[glue::glue("data-{type}")]]
  }
  value
}

# The following functions define inputs including tooltips
# Some are 'fancy' from the shinyWidgets package (e.g., materialSwitch(), etc), and require an extra id to be attached to the label in order to properly display tooltips (see https://github.com/dreamRs/shinyWidgets/issues/63)


## Functions for inputs ------------

select_custom_months <- function(id) {
  fluidRow(id = NS(id, "custom_months_all"),
           h4("Combine and summarize months"),
           column(width = 6,
                  selectizeInput(NS(id, "custom_months"),
                                 label = "Months to combine",
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
                            label = "Label for group",
                            placeholder = "ex. Jun-Aug",
                            value = "")),
           bsTooltip(id = NS(id, "custom_months_all"),
                     title = glue::glue("Months: {tips$custom_months}<br>",
                                        "Label: {tips$custom_months_label}"),
                     placement = "left")
  )
}

select_discharge <- function(id, input = NULL, set = TRUE) {
  selected <- set_input("discharge", input, set, NULL)

  tagList(
    awesomeRadio(NS(id, "discharge"),
                 label = "Discharge type",
                 choices = list("Discharge (cms)" = "Value",
                                "Volumetric Discharge (m3)" = "Volume_m3",
                                "Runoff Yield (mm)" = "Yield_mm"),
                 selected = selected),
    bsTooltip(NS(id, "discharge"), tips$discharge,
              placement = "left"))
}

select_rolling <- function(id, input = NULL, name = "roll",
                           set = TRUE, multiple = FALSE) {
  if(multiple) d <- c(1, 3, 7, 30) else d <- 1
  value <- set_input(glue::glue("{name}_days"), input, set, d)
  selected <- set_input(glue::glue("{name}_align"), input, set, "right")

  tagList(
    fluidRow(id = NS(id, glue::glue("{name}ing")),
             column(6,
                    selectizeInput(NS(id, glue::glue("{name}_days")),
                                   label = "Rolling days",
                                   choices = 1:180,
                                   selected = value,
                                   multiple = multiple)),
             column(6,
                    selectizeInput(NS(id, glue::glue("{name}_align")),
                                   label = "Rolling align",
                                   selected = selected,
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
                               label = "Percentiles to calculate") {

  tagList(
    selectizeInput(NS(id, name),
                   label = label,
                   choices = c(1:99),
                   selected = selected,
                   multiple = TRUE),
    bsTooltip(NS(id, name), tips[[name]], placement = "left"))
}

select_complete <- function(id, input = NULL, set = TRUE) {
  value <- set_input("complete", input, set, FALSE)

  tagList(
    div(id = NS(id, "complete_tip"),
        prettySwitch(NS(id, "complete"),
                     label = "Complete years only",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "complete_tip"), tips$complete,
              placement = "left"))
}

select_missing <- function(id, input = NULL, set = TRUE, value = NULL) {
  value <- set_input("missing", input, set,
                     dplyr::if_else(is.null(value), TRUE, value))

  tagList(
    div(id = NS(id, "missing_tip"),
        prettySwitch(NS(id, "missing"),
                     value = value, status = "danger",
                     label = "Ignore missing values",
                     slim = TRUE)),
    bsTooltip(NS(id, "missing_tip"), tips$missing,
              placement = "left"))
}

select_allowed <- function(id, input = NULL, set = TRUE, value = NULL) {
  value <- set_input("allowed", input, set,
                     dplyr::if_else(is.null(value), 100, value))

  tagList(
    sliderInput(NS(id, "allowed"),
                label = "Allowed missing (%)",
                value = value, step = 5, min = 0, max = 100),
    bsTooltip(NS(id, "allowed"), tips$allowed,
              placement = "left"))
}

select_plot_stats <- function(id, stats) {
  if(!is.null(stats)) {
    tagList(
      radioGroupButtons(NS(id, "stats"),
                        label = "Statistics",
                        choices = stats,
                        selected = stats),
      bsTooltip(NS(id, "stats"), tips$stats,
                placement = "left"))
  }
}

select_plot_log <- function(id, value = TRUE) {

  tagList(
    div(id = NS(id, "plot_log_tip"),
        prettySwitch(NS(id, "plot_log"),
                     label = "Use log scale",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "plot_log_tip"), tips$plot_log,
              placement = "left"))
}

select_plot_extremes <- function(id, value = TRUE) {

  tagList(
    div(id = NS(id, "plot_extremes_tip"),
        prettySwitch(NS(id, "plot_extremes"),
                     label = "Plot extreme values",
                     value = value,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "plot_extremes_tip"), tips$plot_extremes,
              placement = "left"))
}

select_daterange <- function(id, data) {

  if(is.null(data)) stop("Require 'data' to create daterange UI",
                         call. = FALSE)
  dateRangeInput(NS(id, "daterange"),
                 label = "Start/End dates of data to plot",
                 format = "yyyy-mm-dd", startview = "month",
                 start = min(data$Date), end = max(data$Date))
  # Tooltip didn't work? (even with tags$span trick)
}

select_add_year <- function(id, years_range) {
  tagList(
    selectizeInput(NS(id, "add_year"),
                   label = "Year to add",
                   choices = c("Choose a year" = "",
                               seq(from = years_range[1],
                                   to = years_range[2], by = 1)),
                   selected = NULL,
                   multiple = FALSE),
    bsTooltip(NS(id, "add_year"), tips$add_year,
              placement = "left")
  )
}

select_add_dates <- function(id) {
  d <- setNames(1:365, format(as.Date(1:365, origin = "1899-12-31"),
                              "%b-%d"))

  # Start disabled, enable if correct type selected
  # Updated depending on water year in UI section of server.R

  tagList(
    selectizeInput(
      NS(id, "add_dates"),
      label = "Date to show",
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
                     label = "Add MAD values",
                     value = FALSE,
                     status = "success", slim = TRUE)),
    bsTooltip(NS(id, "add_mad_tip"), tips$add_mad,
              placement = "left"))
}

select_plot_display <- function(id, plots) {
  plot_names <- names(plots) %>%
    setNames(., stringr::str_replace_all(., "_", " "))

  tagList(
    selectizeInput(NS(id, "display"), "Display plot",
                   choices = plot_names),
    bsTooltip(NS(id, "display"),
              paste0("Choose plot type to display.<br>",
                     "Seasonal plots are only available if all months ",
                     "are included<br>(see Data tab)"),
              placement = "left"))
}

select_plot_options <- function(...) {
  div(
    align = "right",
    dropdownButton(
      tags$h3("Plot options"),
      tagList(...),
      status = "primary", icon = icon("gear", verify_fa = FALSE),
      size = "sm", width = "300px", right = TRUE,
      tooltip = tooltipOptions(title = "Plot options", placement = "left")
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

show_ui <- function(id, name) {
  h4(materialSwitch(inputId = id,
                    label = name,
                    status = "primary"))
}

