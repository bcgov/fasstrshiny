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
  if(set & !is.null(input[[glue("data_{type}")]])) {
    value <- input[[glue("data_{type}")]]
  }
  value
}

# The following functions define inputs including tooltips
# Some are 'fancy' from the shinyWidgets package (e.g., materialSwitch(), etc), and require an extra id to be attached to the label in order to properly display tooltips (see https://github.com/dreamRs/shinyWidgets/issues/63)


## Functions for inputs ------------

select_custom_months <- function(id) {
  fluidRow(id = glue("{id}_custom_months_all"),
           h4("Combine and summarize months"),
           column(width = 6,
                  selectizeInput(glue("{id}_custom_months"),
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
                  textInput(glue("{id}_custom_months_label"),
                            label = "Label for group",
                            placeholder = "ex. Jun-Aug",
                            value = "")),
           bsTooltip(id = glue("{id}_custom_months_all"),
                     title = glue("Months: {tips$custom_months}<br>",
                                  "Label: {tips$custom_months_label}"),
                     placement = "left")
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
    bsTooltip(glue("{id}_discharge"), tips$discharge,
              placement = "left"))
}

select_rolling <- function(id, input = NULL, set = TRUE, multiple = FALSE) {
  if(multiple) d <- c(1, 3, 7, 30) else d <- 1
  value <- set_input("roll_days", input, set, d)
  selected <- set_input("roll_align", input, set, "right")

  tagList(
    fluidRow(id = glue("{id}_rolling"),
             column(6,
                    selectizeInput(glue("{id}_roll_days"),
                                   label = "Rolling days",
                                   choices = 1:180,
                                   selected = value,
                                   multiple = multiple)),
             column(6,
                    selectizeInput(glue("{id}_roll_align"),
                                   label = "Rolling align",
                                   selected = selected,
                                   choices = list("Right" = "right",
                                                  "Left" = "left",
                                                  "Center" = "center")))
    ),
    bsTooltip(id = glue("{id}_rolling"),
              title = glue("Days: {tips$roll_days}<br>Align: {tips$roll_align}"),
              placement = "left"))
}


select_percentiles <- function(id, selected = c(10, 90),
                               label = "Percentiles to calculate") {
  tagList(
    selectizeInput(glue("{id}_percentiles"),
                   label = label,
                   choices = c(1:99),
                   selected = selected,
                   multiple = TRUE),
    bsTooltip(glue("{id}_percentiles"), tips$percentiles,
              placement = "left"))
}

select_complete <- function(id, input = NULL, set = TRUE) {
  value <- set_input("complete", input, set, FALSE)

  tagList(
    prettySwitch(glue("{id}_complete"),
                 label = tags$span("Complete years only",
                                   id = glue("{id}_complete_tip")),
                 value = value,
                 status = "success", slim = TRUE),
    bsTooltip(glue("{id}_complete_tip"), tips$complete,
              placement = "left"))
}

select_missing <- function(id, input = NULL, set = TRUE, value = NULL) {
  value <- set_input("missing", input, set,
                     if_else(is.null(value), TRUE, value))

  tagList(
    prettySwitch(glue("{id}_missing"),
                 value = value, status = "danger",
                 label = tags$span("Ignore missing values",
                                   id = glue("{id}_missing_tip")),
                 slim = TRUE),
    bsTooltip(glue("{id}_missing_tip"), tips$missing,
              placement = "left"))
}

select_allowed <- function(id, input = NULL, set = TRUE, value = NULL) {
  value <- set_input("allowed", input, set,
                     if_else(is.null(value), 100, value))

  tagList(
    sliderInput(glue("{id}_allowed"),
                label = "Allowed missing (%)",
                value = value, step = 5, min = 0, max = 100),
    bsTooltip(glue("{id}_allowed"), tips$allowed,
              placement = "left"))
}

select_plot_stats <- function(id, stats) {
  if(!is.null(stats)) {
    tagList(
      radioGroupButtons(glue("{id}_stats"),
                        label = "Statistics",
                        choices = stats,
                        selected = stats),
      bsTooltip(glue("{id}_stats"), tips$stats,
                placement = "left"))
  }
}

select_plot_log <- function(id, value = TRUE) {

  tagList(
    prettySwitch(glue("{id}_plot_log"),
                 label = tags$span("Use log scale",
                                   id = glue("{id}_plot_log_tip")),
                 value = value,
                 status = "success", slim = TRUE),
    bsTooltip(glue("{id}_plot_log_tip"), tips$plot_log,
              placement = "left"))
}

select_plot_extremes <- function(id, value = TRUE) {

  tagList(
    prettySwitch(glue("{id}_plot_extremes"),
                 label = tags$span("Plot extreme values",
                                   id = glue("{id}_plot_extremes_tip")),
                 value = value,
                 status = "success", slim = TRUE),
    bsTooltip(glue("{id}_plot_extremes_tip"), tips$plot_extremes,
              placement = "left"))
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
    bsTooltip(glue("{id}_add_year"), tips$add_year,
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
      glue("{id}_add_dates"),
      label = "Date to show",
      choices = c("Choose date(s)" = "", d),
      selected = NULL, multiple = TRUE),
    bsTooltip(glue("{id}_add_dates"), tips$add_dates,
              placement = "left")
  )
}


select_add_mad <- function(id) {
  tagList(
    prettySwitch(glue("{id}_add_mad"),
                 label = tags$span("Add MAD values",
                                   id = glue("{id}_add_mad_tip")),
                 value = FALSE,
                 status = "success", slim = TRUE),
    bsTooltip(glue("{id}_add_mad_tip"), tips$add_mad,
              placement = "left"))
}

show <- function(id, name) {
  h4(materialSwitch(inputId = id,
                    label = name,
                    status = "primary"))
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


## Generic Functions -----------------------------------

check_data <- function(input){
  validate(need(input$data_load,
                "You'll need to first load some data under Data > Loading"))
}

# For multiple, sequential validates
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}


build_ui <- function(id, input = NULL, define_options = FALSE, include) {

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
#' This function looks for common inputs into fasstr functions. It uses general
#' inputs prefaced by `data_` unless a specific input prefaced by the values of
#' `id` exists.
#'
#' For example, `fun = "plot_daily_stats"` would use `input$data_months`,
#' because there is no `input$hydro_months`. On the other hand,
#' `fun = "plot_missing_dates"` would use `input$available_months` before
#' `input$data_months`.
#'
#' You can specify inputs to ignore with `params_ignore`. This can be useful
#' when you need to combine inputs, for example, `percentiles` are combined
#' in daily hydrograph tables (e.g., `calc_daily_stats()`). It is also
#' useful to ignore `discharge` where it's not sensible to have (i.e.
#' `plot_daily_cumulative_stats()`).
#'
#' Note that the `remove_defaults` function is used to compare parameter values
#' to their default values. Default values are removed to simplify the code
#' output (this only applies to parameters defined directly in `create_fun`, not
#' parameters defined in the argument `extra`).
#'
#'
#' @param fun Character. Name of the fasstr function to use
#' @param data Character. Name of the data to use
#' @param id Character. Input/output id (e.g., "hydro")
#' @param input Shiny input object
#' @param extra Character. String adding extra arguments unrelated to the
#'   common parameters defined at the end of `create_fun`
#' @param end Character. String to put after the function (e.g., " %>% ...")
#'
#' @return String defining the function. Can be evaluated with `eval(parse(x))`.
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' t <- create_fun(fun = "calc_longterm_daily_stats",
#'                 data = "flow_data", id = "hyrdo", input)
#'
#'
#' t <- create_fun(fun = "calc_longterm_mean",
#'                 data = "flow_data", id = "sumsi", input,
#'                 params_extra = c("mad" = "percent_MAD = c(input$sumsi_mad)"))
#' }

create_fun <- function(fun, data = NULL, id, input,
                       params_ignore = NULL, extra = NULL, end = "") {

  # Inputs from Data tab
  params_data <- c("discharge", "water_year", "years_range", "years_exclude",
                   "roll_days", "roll_align", "months",
                   "missing", "complete", "allowed") %>%
    setNames(rep("data", length(.)))

  # Inputs from this section (id)
  params_id <- str_subset(names(input), glue("^{id}_")) %>%
    str_remove(glue("^{id}_")) %>%
    setNames(rep(id, length(.)))

  # Inputs expected by the function (getting the app input equivalents)
  params_fun <- names(formals(get(fun)))
  params_fun <- filter(parameters, fasstr_arg %in% params_fun) %>%
    pull(id) %>%
    unique()

  # Combine inputs but
  # - only data not in id
  # - only ones expected by the function
  params <- c(params_data[!params_data %in% params_id], params_id)
  params <- params[params %in% params_fun]

  # Omit any to be ignored
  if(!is.null(params_ignore)) params <- params[!params %in% params_ignore]

  # Retrieve inputs for these parameters
  names(params) <- glue("{names(params)}_{params}")
  values <- map(names(params), ~input[[.]])

  # Remove NULL/empty
  nulls <- map_lgl(values, ~is.null(.) || (is.character(.) && . == ""))
  values <- values[!nulls]
  params <- params[!nulls]

  # Find and remove defaults
  defaults <- remove_defaults(fun, params, input)
  params <- params[!defaults]
  values <- values[!defaults]

  # If we have allowed_missing (allowed), omit ignore_missing (missing)
  if("allowed" %in% params) {
    values <- values[params != "missing"]
    params <- params[params != "missing"]
  }

  # Put it all together
  p <- combine_parameters(params)
  args <- glue_collapse(c(data, na.omit(p), extra), sep = ', ')

  glue("{fun}({args}){end}")
}

combine_parameters <- function(params) {
  # Create standard parameters
  #
  # - REMEMBER! When collapsing multiple elements with glue_collapse, use [[i]]
  p <- vector()
  for(i in seq_along(params)) {
    p[i] <- case_when(

      # Specific
      params[i] == "discharge" ~ glue("values = '{values[i]}'"),
      params[i] == "percentiles" ~
        glue("percentiles = c({glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "inner_percentiles" ~
        glue("inner_percentiles = c({glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "outer_percentiles" ~
        glue("outer_percentiles = c({glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "custom_months" ~
        glue("custom_months = c({glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "custom_months_label" ~ glue("custom_months_label = '{values[i]}'"),
      params[i] == "missing" ~ glue("ignore_missing = {values[i]}"),
      params[i] == "allowed" ~ glue("allowed_missing = {values[i]}"),
      params[i] == "complete" ~ glue("complete_years = {values[i]}"),
      params[i] == "plot_extremes" ~ glue("include_extremes = {values[i]}"),

      # Data
      params[i] == "water_year" ~
        glue("water_year_start = {values[i]}"),
      params[i] == "years_range" ~
        glue("start_year = {values[[i]][1]}, end_year = {values[[i]][2]}"),
      params[i] == "years_exclude" ~
        glue("exclude_years = c({glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "roll_days" ~ glue("roll_days = {values[i]}"),
      params[i] == "roll_align" ~ glue("roll_align = '{values[i]}'"),
      params[i] == "months" ~
        glue("months = c({glue_collapse(values[[i]], sep = ', ')})"),

      # Plot
      params[i] == "daterange" ~
        glue("start_date = '{values[[i]][1]}', end_date = '{values[[i]][2]}'"),
      params[i] == "plot_log" ~ glue("log_discharge = {values[i]}")
    )
  }
  p
}



remove_defaults <- function(fun, params, input) {

  # Get defaults (omitting symbols)
  defaults <- as.list(formals(get(fun))) %>%
    map(filter_type) %>% # Remove symbols, code refering to other args and eval
    tibble::enframe(name = "fasstr_arg", value = "default")

  input_values <- map(names(params), ~input[[.]]) %>%
    setNames(params) %>%
    tibble::enframe(name = "id", value = "input")

   id <- parameters %>%
     left_join(defaults, by = "fasstr_arg") %>%
     left_join(input_values, by = "id") %>%
     select(id, default, input) %>%
     mutate(same = map2_lgl(default, input, params_equal)) %>%
     filter(same) %>%
     pull(id)

   # Return default params
   params %in% id
}

filter_type <- function(x) {
  if(typeof(x) != "symbol") {
    x <- try(eval(x), silent = TRUE)
    if("try-error" %in% class(x)) x <- NULL
  } else x <- NULL
  x
}


params_equal <- function(x, y) {
  if(length(x) > 0 & length(y) > 0) {
    all.equal(sort(as.character(x)), sort(as.character(y)))[1] == TRUE
  } else FALSE
}
