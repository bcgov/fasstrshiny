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

# Functions --------------------------------------------


# Determine the default value, if input exists and set is TRUE, returns
# global options preset, otherwise default
set_input <- function(type, input, set, value) {
  if(set & !is.null(input)) {
    value <- input[[glue("opts_{type}")]]
  }
  value
}

# The following functions define inputs including tooltips
# Some are 'fancy' from the shinyWidgets package (e.g., materialSwitch(), etc), and require an extra id to be attached to the label in order to properly display tooltips (see https://github.com/dreamRs/shinyWidgets/issues/63)


## Functions for inputs ------------
select_months <- function(id, input = NULL, set = TRUE) {
  selected <- set_input("month", input, set, 1:12)

  tagList(
    selectizeInput(glue("{id}_months"),
                   label = "Months to Include",
                   choices = list("Jan" = 1,  "Feb" = 2,
                                  "Mar" = 3,  "Apr" = 4,
                                  "May" = 5,  "Jun" = 6,
                                  "Jul" = 7,  "Aug" = 8,
                                  "Sep" = 9,  "Oct" = 10,
                                  "Nov" = 11, "Dec" = 12),
                   selected = selected,
                   multiple = TRUE),
    bsTooltip(glue("{id}_months"), tips$months)
  )
}

select_custom_months <- function(id, input = NULL, set = TRUE) {
  selected <- set_input("custom_months", input, set, NULL)
  value <- set_input("custom_months_label", input, set, "")

  tagList(
    selectizeInput(glue("{id}_custom_months"),
                   label = "Months to combine and summarize",
                   choices = list("Jan" = 1,  "Feb" = 2,
                                  "Mar" = 3,  "Apr" = 4,
                                  "May" = 5,  "Jun" = 6,
                                  "Jul" = 7,  "Aug" = 8,
                                  "Sep" = 9,  "Oct" = 10,
                                  "Nov" = 11, "Dec" = 12),
                   selected = selected,
                   multiple = TRUE),
    bsTooltip(glue("{id}_custom_months"), tips$custom_months),

    textInput(glue("{id}_custom_months_label"),
              label = "Summary months label",
              placeholder = "ex. Jun-Aug",
              value = value),
    bsTooltip(glue("{id}_custom_months_label"), tips$custom_months_label)
  )
}

select_discharge <- function(id, input = NULL, set = TRUE) {
  selected <- set_input("discharge", input, set, NULL)

  tagList(
    awesomeRadio(glue("{id}_discharge"),
                 label = "Discharge type",
                 choices = list("Discharge (cms)" = "Value",
                                "Volumetric Discharge (m3)" = "Volume_m3",
                                "Runoff Yield (mm)" = "Yield_mm"),
                 selected = selected),
    bsTooltip(glue("{id}_discharge"), tips$discharge, placement = "left"))
}

select_rolling <- function(id, input = NULL, set = TRUE, multiple = FALSE) {
  if(multiple) d <- c(1, 3, 7, 30) else d <- 1
  value <- set_input("roll_days", input, set, d)
  selected <- set_input("roll_align", input, set, "right")

  fluidRow(
    column(6,
           selectizeInput(glue("{id}_roll_days"),
                          label = "Rolling days",
                          choices = 1:180,
                          selected = value,
                          multiple = multiple),
           bsTooltip(glue("{id}_roll_days"), tips$roll_days)),
    column(6,
           selectizeInput(glue("{id}_roll_align"),
                          label = "Rolling align",
                          selected = selected,
                          choices = list("Right" = "right",
                                         "Left" = "left",
                                         "Center" = "center")),
           bsTooltip(glue("{id}_roll_align"), tips$roll_align)),
  )
}


select_percentiles <- function(id, input = NULL, set = TRUE) {
  selected <- set_input("percentiles", input, set, c(10, 90))

  tagList(
    selectizeInput(glue("{id}_percentiles"),
                   label = "Percentiles to calculate",
                   choices = c(1:99),
                   selected = selected,
                   multiple = TRUE),
    bsTooltip(glue("{id}_percentiles"), tips$percentiles))
}

select_complete <- function(id, input = NULL, set = TRUE) {
  value <- set_input("complete", input, set, FALSE)

  if(id == "sum") {
    tip <- glue("{tips$complete}<br>(applies only to long-term, ",
                "daily and MAD calculations")
  } else {
    tip <- tips$complete
  }

  tagList(
    materialSwitch(glue("{id}_complete"),
                   label = tags$span("Complete years only",
                                    id = glue("{id}_complete_tip")),
                   value = value,
                   status = "success"),
    bsTooltip(glue("{id}_complete_tip"), tip))
}

select_missing <- function(id, input = NULL, set = TRUE, value = NULL) {
  value <- set_input("missing", input, set,
                     if_else(is.null(value), TRUE, value))

  tagList(
    materialSwitch(glue("{id}_missing"),
                   value = value, status = "danger",
                   label = tags$span("Ignore missing values",
                                     id = glue("{id}_missing_tip"))),
    bsTooltip(glue("{id}_missing_tip"), tips$missing))
}

select_allowed <- function(id, input = NULL, set = TRUE, value = NULL) {
  value <- set_input("allowed", input, set,
                     if_else(is.null(value), 100, value))

  tagList(
    sliderInput(glue("{id}_allowed"),
                label = "Allowed missing (%)",
                value = value, step = 5, min = 0, max = 100),
    bsTooltip(glue("{id}_allowed"), tips$allowed))
}

# Special one for when there is a "type" to calculate
select_miss_allowed <- function(id, input = NULL, set = TRUE) {
  req(input[[glue("{id}_type")]])

  if(input[[glue("{id}_type")]] %in% c("Long-term", "Daily")) {
    select_missing(id, input, set, value = input[[glue("{id}_missing")]])
  } else {
    select_allowed(id, input, set, value = input[[glue("{id}_allowed")]])
  }
}

select_plot_stats <- function(id, stats) {
  if(!is.null(stats)) {
    tagList(
      radioGroupButtons(glue("{id}_stats"),
                        label = "Statistics",
                        choices = stats,
                        selected = stats),
      bsTooltip(glue("{id}_stats"), tips$stats))
  }
}

select_plot_log <- function(id, input = NULL, set = TRUE) {
  value <- set_input("plot_log", input, set, TRUE)

  tagList(
    materialSwitch(glue("{id}_plot_log"),
                   label = tags$span("Use log scale",
                                     id = glue("{id}_plot_log_tip")),
                   value = value,
                   status = "success"),
    bsTooltip(glue("{id}_plot_log_tip"), tips$plot_log))
}

select_daterange <- function(id, data) {

  if(is.null(data)) stop("Require 'data' to create daterange UI",
                         call. = FALSE)

  dateRangeInput(glue("{id}_daterange"),
                 label = "Start/End dates of data to plot",
                 format = "yyyy-mm-dd", startview = "month",
                 start = min(data$Date), end = max(data$Date))
  # Tooltip didn't work? (even with tags$span trick)
}

select_add_year <- function(id, input) {
  req(input$data_years_range)
  tagList(
    selectizeInput(glue("{id}_add_year"),
                   label = "Year to add",
                   choices = c("Choose a year" = "",
                               seq(from = input$data_years_range[1],
                                   to = input$data_years_range[2], by = 1)),
                   selected = NULL,
                   multiple = FALSE),
    bsTooltip(glue("{id}_add_year"), tips$add_year)
  )
}

select_add_dates <- function(id) {
  d <- setNames(1:365, format(as.Date(1:365, origin = "1899-12-31"),
                              "%b-%d"))

  # Start disabled, enable if correct type selected
  # Updated depending on water year in UI section of server.R

  tagList(
    disabled(selectizeInput(
      "sum_add_dates",
      label = "Date to show",
      choices = c("Choose date(s)" = "", d),
      selected = NULL, multiple = TRUE)),
    bsTooltip(glue("{id}_add_dates"), tips$add_dates))
}


select_add_mad <- function(id) {
  tagList(
    materialSwitch(glue("{id}_mad_add"),
                   label = tags$span("Add MAD values",
                                     id = glue("{id}_mad_add_tip")),
                   value = FALSE,
                   status = "success"),
    bsTooltip(glue("{id}_mad_add_tip"), tips$add_mad))
}

select_plot_options <- function(id, input, include = "plot_log",
                                params = NULL, data = NULL) {

  i <- tagList()
  if("plot_log" %in% include) i <- tagList(i, select_plot_log(id, input))
  if("percentiles" %in% include) i <- tagList(i, select_percentiles(id, input))
  if("daterange" %in% include) i <- tagList(i, select_daterange(id, data))
  if("discharge" %in% include) i <- tagList(i, select_discharge(id, input))
  if("plot_stats" %in% include) i <- tagList(i, select_plot_stats(id, params))
  if("add_year" %in% include) i <- tagList(i, select_add_year(id, input))
  if("add_dates" %in% include) i <- tagList(i, select_add_dates(id))
  if("add_mad" %in% include) i <- tagList(i, select_add_mad(id))

  t <- dropdownButton(
    tags$h3("Plot options"),
    i,
    status = "primary", icon = icon("gear", verify_fa = FALSE),
    size = "sm", width = "300px", right = TRUE,
    tooltip = tooltipOptions(title = "Plot options", placement = "left")
  )
}

select_table_options <- function(id, input,
                                 include = c("percentiles", "custom_months"),
                                 params = NULL, data = NULL) {

  i <- tagList()
  if("percentiles" %in% include) i <- tagList(i, select_percentiles(id, input))
  if("custom_months" %in% include) i <- tagList(i, select_custom_months(id, input))

  t <- dropdownButton(
    tags$h3("Table options"),
    i,
    status = "primary", icon = icon("gear", verify_fa = FALSE),
    size = "sm", width = "300px", right = TRUE,
    tooltip = tooltipOptions(title = "Table options", placement = "left")
  )
}


## Generic Functions -----------------------------------

build_ui <- function(id, input = NULL, define_options = FALSE,
                     include, global = global_settings) {

  if(any(global %in% include) & !define_options){
    stop("Some ui elements included here (",
         paste0(include[include %in% c("rolling", "months")],
                collapse = ", "),
         ") should only be set once on the 'Settings' tab.\n",
         "They should not be set on individual tabs.",
         call. = FALSE)
  }

  # Set up all options in the settings
  # - Don't set defaults
  if(define_options == TRUE) {
    set <- FALSE
    ui <- include %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList)
  } else {
    # Set up options for a specific tab
    # - Set defaults from the Settings tab
    set <- TRUE
    ui <- include %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList)
  }
  ui
}


#' Combine elements for functions
#'
#' @param fun Character. Name of the fasstr function to use
#' @param data Character. Name of the data to use
#' @param id Character. Input/output id (e.g., "sum")
#' @param input Shiny input object
#' @param params Character vector of inputs to become arguments. Unnamed inputs
#'   are assumed to be "id_input". Any input not part of id can be named (e.g.,
#'   "data" = "water_year").
#' @param extra Character. String with extra arguments not related to shiny
#'   inputs
#' @param end Character. String to put after the function (e.g., "[[1]]" for
#'   plots)
#'
#' @return
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' t <- create_fun(fun = "calc_longterm_daily_stats",
#'                 data = "flow_data", id = "sumfl", input,
#'                 params = c("discharge", "roll_days", "roll_align",
#'                            "data" = "water_year",
#'                            "data" = "years_range",
#'                            "data" = "years_exclude",
#'                            "months", "missing"))
#'
#'
#' t <- create_fun(fun = "calc_longterm_mean",
#'                 data = "flow_data", id = "sumsi", input,
#'                 params = c("discharge", "roll_days", "roll_align",
#'                            "water_year", "years_range", "years_exclude",
#'                            "months", "mad"),
#'                params_extra = c("mad" = "percent_MAD = c(input$sumsi_mad)"))
#' }

create_fun <- function(fun, data = NULL, id, input, params = NULL,
                       params_ignore = NULL, extra = NULL, end = "") {

  params_default <- c("discharge", "roll_days", "roll_align", "water_year",
                      "years_range", "years_exclude", "months")

  if(!is.null(params_ignore)) {
    params_default <- params_default[!params_default %in% params_ignore]
  }

  params <- unique(c(params, params_default))

  # Figure out where parameters come from
  if(is.null(names(params))) {
    n <- rep(NA_character_, length(params))
  } else n <- names(params)
  n[params %in% c("roll_days", "roll_align", "months")] <- "opts"
  n[params %in% c("water_year", "years_range", "years_exclude")] <- "data"
  n[is.na(n)] <- id
  names(params) <- glue("{n}_{params}")

  # Retrieve inputs for these parameters
  id <- map(names(params), ~input[[.]])

  # Remove NULL/empty
  nulls <- map_lgl(id, ~is.null(.) || (is.character(.) && . == ""))
  id <- id[!nulls]
  params <- params[!nulls]

  # Create standard parameters
  #
  # - REMEMBER! When collapsing multiple elements with glue_collapse, use [[i]]
  p <- vector()
  for(i in seq_along(params)) {
    p[i] <- case_when(

      # Specific
      params[i] == "discharge" ~ glue("values = '{id[i]}'"),
      params[i] == "percentiles" ~
        glue("percentiles = c({glue_collapse(id[[i]], sep = ', ')})"),
      params[i] == "custom_months" ~
        glue("custom_months = c({glue_collapse(id[[i]], sep = ', ')})"),
      params[i] == "custom_months_label" ~ glue("custom_months_label = '{id[i]}'"),
      params[i] == "missing" ~ glue("ignore_missing = {id[i]}"),
      params[i] == "allowed" ~ glue("allowed_missing = {id[i]}"),
      params[i] == "complete" ~ glue("complete_years = {id[i]}"),

      # Opts
      params[i] == "roll_days" ~ glue("roll_days = {id[i]}"),
      params[i] == "roll_align" ~ glue("roll_align = '{id[i]}'"),
      params[i] == "months" ~
        glue("months = c({glue_collapse(id[[i]], sep = ', ')})"),

      # Data
      params[i] == "water_year" ~
        glue("water_year_start = {id[i]}"),
      params[i] == "years_range" ~
        glue("start_year = {id[[i]][1]}, end_year = {id[[i]][2]}"),
      params[i] == "years_exclude" ~
        glue("exclude_years = c({glue_collapse(id[[i]], sep = ', ')})"),

      # Plot
      params[i] == "daterange" ~
        glue("start_date = '{id[[i]][1]}', end_date = '{id[[i]][2]}'"),
      params[i] == "plot_log" ~ glue("log_discharge = {id[i]}")
    )
  }


  args <- glue_collapse(c(data, p, extra), sep = ', ')

  glue("{fun}({args}){end}")
}
