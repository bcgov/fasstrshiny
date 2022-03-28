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
#' @return String defining the function. Can be evaluated with `eval_check()`.
#'
#' @noRd
#'
#'

create_fun <- function(fun, data_name = NULL, input, input_data = NULL,
                       params_ignore = NULL, extra = "", end = "") {

  # Use isolate() when references all of input, but leave specific
  # input references with input$ and input[[]] open to update if that input
  # changes

  params_data <- names(input_data)    # Inputs from Data tab
  params_mod <- isolate(names(input)) # Inputs from this module

  # Inputs expected by the function (getting the app input equivalents)
  params_fun <- names(formals(fun))
  params_fun <- dplyr::filter(parameters,
                              .data$fasstr_arg %in% .env$params_fun) %>%
    dplyr::pull(.data$id) %>%
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

  values_data <- purrr::map(params_data, ~input_data[[.]]) %>%
    stats::setNames(params_data)
  values_mod <- purrr::map(params_mod, ~input[[.]]) %>%
    stats::setNames(params_mod)

  # Join
  values <- append(values_data, values_mod)

  # Keep some NULLs
  values <- keep_null(values, c("inner_percentiles", "outer_percentiles"))

  # Remove NULL/empty (except those to keep)
  nulls <- purrr::map_lgl(values, ~is.null(.) || (is.character(.) && . == ""))
  values <- values[!nulls]

  # Mark missing inputs as NULL
  mark_null(values, c("add_year"))

  # Find and remove defaults
  defaults <- remove_defaults(fun, input_values = values)
  values <- values[!defaults]

  # If we have allowed_missing (allowed), omit ignore_missing (missing)
  if("allowed" %in% names(values)) {
    values <- values[names(values) != "missing"]
  }

  # Put it all together
  p <- purrr::imap(values, ~combine_parameters(.y, .x))

  if(extra == "") extra <- NULL
  args <- glue::glue_collapse(c(data_name, stats::na.omit(p), extra), sep = ', ')

  glue::glue("{fun}({args}){end}")
}

mark_null <- function(values, args) {
  for(a in args) {
    if(a %in% names(values) && values[[a]] == "") values[[a]] <- NULL
  }
  values
}

keep_null <- function(values, args) {
  for(a in args) {
    if(a %in% names(values) && is.null(values[[a]])) {
      values[[a]] <- "NULL"
    }
  }
  values
}


#' Create standard parameters
#'
#' Determine how parameters should be written as fasstr arguments.
#'
#' Strictly speaking, most of this function could be an internal data frame,
#' created in parameters.R, but it might be easier to keep it here, close
#' to `create_fun()`.
#'
#' - All parameters here, must be listed in data-raw/parameters.R
#'  (and when adding to that file, you'll have to re-run it and re-load
#'  the package)
#'
#' - Note that character values (i.e. custom_months_label) need to be surrounded
#'   by ' '
#' - Can use conseq() for any numeric (or month character, with type = "months")
#'   to collapse numbers
#'
#' @noRd

combine_parameters <- function(p, v) {

  v <- sort(v)

  dplyr::tribble(
    ~param,                ~glued,
    "longterm",            "include_longterm = {v}",
    "percentiles",         "percentiles = {conseq(v)}",
    "inner_percentiles",   "inner_percentiles = {conseq(v)}",
    "outer_percentiles",   "outer_percentiles = {conseq(v)}",
    "normal_percentiles",  "normal_percentiles = {conseq(v)}",

    "custom_months",       "custom_months = {conseq(v)}",
    "custom_months_label", "custom_months_label = '{v}'",
    "missing",             "ignore_missing = {v}",
    "allowed",             "allowed_missing = {v}",
    "complete",            "complete_years = {v}",
    "mad",                 "percent_MAD = {conseq(v)}",
    "percent",             "percent_total = {conseq(v)}",

    # Data
    "discharge",           "values = '{v}'",
    "discharge2",          "use_yield = {v}",
    "basin_area",          "basin_area = {v}",
    "water_year",          "water_year_start = {v}",
    "years_range",         "start_year = {v[1]}, end_year = {v[2]}",
    "years_exclude",       "exclude_years = {conseq(v)}",
    "roll_days",           "roll_days = {conseq(v)}",
    "roll_align",          "roll_align = '{v}'",
    "months",              "months = {conseq(v)}",

    # Plot
    "stats",  "include_stats = c(\"{glue::glue_collapse(v, sep = '\", \"')}\")",

    "availability",        "plot_availability = {v}",
    "symbols_percent",     "plot_percent = {v}",
    "daterange",           "start_date = '{v[1]}', end_date = '{v[2]}'",
    "plot_log",            "log_discharge = {v}",
    "plot_extremes",       "include_extremes = {v}",
    "add_year",            "add_year = {v}",

    # Analysis
    "zyp",                 "zyp_method = '{v}'",
    "annual_percentiles",  "annual_percentiles = {conseq(v)}",
    "monthly_percentiles", "monthly_percentiles = {conseq(v)}",
    "low_roll_days",       "lowflow_days = {conseq(v)}",
    "low_roll_align",      "lowflow_align = '{v}'",
    "allowed_annual",      "allowed_missing_annual = {v}",
    "allowed_monthly",     "allowed_missing_monthly = {v}",
    "timing_percent",      "timing_percent = {conseq(v)}",
    "alpha",               "zyp_alpha = {v}",

    "use_max",             "use_max = {v}",
    "use_log",             "use_log = {v}",
    "prob_plot",           "prob_plot_position = '{v}'",
    "prob_scale",          "prob_scale_points = c({v})",
    "fit_distr",           "fit_distr = '{v}'",
    "fit_quantiles",       "fit_quantiles = {conseq(v)}",
    "fit_distr_method",    "fit_distr_method = '{v}'",
    "plot_curve",          "plot_curve = {v}") %>%

    dplyr::filter(.data$param == .env$p) %>%
    dplyr::pull(.data$glued) %>%
    glue::glue()
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
    dplyr::select("id", "default", "input") %>%
    dplyr::mutate(same = purrr::map2_lgl(
      .data$default, .data$input, params_equal)) %>%
    dplyr::filter(.data$same) %>%
    dplyr::pull(.data$id)

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
