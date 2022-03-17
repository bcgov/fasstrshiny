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




## Generic Functions -----------------------------------


prep_DT <- function(data, digits = 4) {
  data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., digits))) %>%
    DT::datatable(rownames = FALSE,
                  filter = 'top',
                  extensions = c("Scroller"),
                  options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                                 deferRender = TRUE, dom = 'Brtip'))
}


stop_ui_suspend <- function(id, output) {
  names(outputOptions(output)) %>%
    stringr::str_subset(glue::glue("{id}-ui_")) %>%
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
