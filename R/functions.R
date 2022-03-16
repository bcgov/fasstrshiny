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

show_ui <- function(id, name) {
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

ui_rcode <- function(id) {
  tabPanel(title = "R Code", verbatimTextOutput(NS(id, "code")))
}

prep_DT <- function(data, digits = 4) {
  data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., digits))) %>%
    DT::datatable(rownames = FALSE,
                  filter = 'top',
                  extensions = c("Scroller"),
                  options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                                 deferRender = TRUE, dom = 'Brtip'))
}


stop_ui_suspend <- function(output) {
  names(outputOptions(output)) %>%
    stringr::str_subset("ui_") %>%
    stringr::str_extract("ui_(.)+$") %>%
    purrr::map(~outputOptions(output, ., suspendWhenHidden = FALSE))
}



check_data <- function(x){
  validate(need(x,"You'll need to first load some data under Data > Loading"))
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

create_fun <- function(fun, data_name = NULL, input, input_data = NULL,
                       params_ignore = NULL, extra = "", end = "") {

  # Use isolate() when references all of input, but leave specific
  # input references with input$ and input[[]] open to update if that input
  # changes

  params_data <- names(input_data)    # Inputs from Data tab
  params_mod <- isolate(names(input)) # Inputs from this module

  # Inputs expected by the function (getting the app input equivalents)
  params_fun <- names(formals(fun))
  params_fun <- dplyr::filter(parameters, fasstr_arg %in% params_fun) %>%
    dplyr::pull(id) %>%
    unique()

  # Filter params
  # - only data settings not overridden in module settings
  # - only ones expected by the function
  params_data <- params_data[!params_data %in% params_mod &
                               params_data %in% params_fun]
  params_mod <- params_mod[params_mod %in% params_fun]

  # Omit any to be ignored
  if(!is.null(params_ignore)) {
    params_data <- params_data[!params_data %in% params_ignore]
    params_mod <- params_mod[!params_mod %in% params_ignore]
  }

  # Retrieve inputs for these parameters
  # (data + mod, but only data where not in mod)

  values_data <- purrr::map(params_data, ~input_data[[.]])
  values_mod <- purrr::map(params_mod, ~input[[.]])

  # Join
  values <- append(values_data, values_mod)
  params <- c(params_data, params_mod)

  # Remove NULL/empty
  nulls <- purrr::map_lgl(values, ~is.null(.) || (is.character(.) && . == ""))
  values <- values[!nulls]
  params <- params[!nulls]

  # Find and remove defaults
  defaults <- remove_defaults(fun, input_values = setNames(values, params))
  params <- params[!defaults]
  values <- values[!defaults]

  # If we have allowed_missing (allowed), omit ignore_missing (missing)
  if("allowed" %in% params) {
    values <- values[params != "missing"]
    params <- params[params != "missing"]
  }

  # Put it all together
  p <- combine_parameters(params, values)
  if(extra == "") extra <- NULL
  args <- glue::glue_collapse(c(data_name, na.omit(p), extra), sep = ', ')

  glue::glue("{fun}({args}){end}")
}



combine_parameters <- function(params, values) {
  # Create standard parameters
  #
  # - REMEMBER! When collapsing multiple elements with glue_collapse, use [[i]]
  p <- vector()
  for(i in seq_along(params)) {
    p[i] <- dplyr::case_when(

      # Specific
      params[i] == "discharge" ~ glue::glue("values = '{values[i]}'"),
      params[i] == "percentiles" ~
        glue::glue("percentiles = c({glue::glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "inner_percentiles" ~
        glue::glue("inner_percentiles = c({glue::glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "outer_percentiles" ~
        glue::glue("outer_percentiles = c({glue::glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "custom_months" ~
        glue::glue("custom_months = c({glue::glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "custom_months_label" ~ glue::glue("custom_months_label = '{values[i]}'"),
      params[i] == "missing" ~ glue::glue("ignore_missing = {values[i]}"),
      params[i] == "allowed" ~ glue::glue("allowed_missing = {values[i]}"),
      params[i] == "complete" ~ glue::glue("complete_years = {values[i]}"),
      params[i] == "plot_extremes" ~ glue::glue("include_extremes = {values[i]}"),

      # Data
      params[i] == "water_year" ~
        glue::glue("water_year_start = {values[i]}"),
      params[i] == "years_range" ~
        glue::glue("start_year = {values[[i]][1]}, end_year = {values[[i]][2]}"),
      params[i] == "years_exclude" ~
        glue::glue("exclude_years = c({glue::glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "roll_days" ~ glue::glue("roll_days = c({glue::glue_collapse(values[[i]], sep = ', ')})"),
      params[i] == "roll_align" ~ glue::glue("roll_align = '{values[i]}'"),
      params[i] == "months" ~
        glue::glue("months = c({glue::glue_collapse(values[[i]], sep = ', ')})"),

      # Plot
      params[i] == "daterange" ~
        glue::glue("start_date = '{values[[i]][1]}', end_date = '{values[[i]][2]}'"),
      params[i] == "plot_log" ~ glue::glue("log_discharge = {values[i]}")
    )
  }
  p
}



remove_defaults <- function(fun, input_values) {

  # Get defaults (omitting symbols)
  defaults <- as.list(formals(get(fun))) %>%
    # Remove symbols, code referring to other args and eval
    purrr::map(filter_type) %>%
    tibble::enframe(name = "fasstr_arg", value = "default")

  input_values <- tibble::enframe(input_values, name = "id", value = "input")

  id <- parameters %>%
    dplyr::left_join(defaults, by = "fasstr_arg") %>%
    dplyr::left_join(input_values, by = "id") %>%
    dplyr::select(id, default, input) %>%
    dplyr::mutate(same = purrr::map2_lgl(default, input, params_equal)) %>%
    dplyr::filter(same) %>%
    dplyr::pull(id)

  # Return default params
  input_values$id %in% id
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
