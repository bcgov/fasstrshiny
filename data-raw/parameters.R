## code to prepare `parameters` dataset goes here
library(dplyr)
library(glue)

parameters <- tribble(
  ~id,                    ~fasstr_arg,           ~tooltip,                             ~add_arg,
  "stats",                "",                    "Choose statistic to display",       FALSE,
  "add_dates",            "",                    "Choose dates to highlight on the plot",   FALSE,
  "add_mad",              "",                    "Add the calculated Mean Annual Discharge percentiles to the plot", FALSE,

  "discharge",            "values",              "Discharge to use in calculations",   FALSE,
  "discharge",            "use_yield",           "Discharge to use in calculations",   FALSE,
  "roll_days",            "roll_days",           "Number of days over which to roll the mean", TRUE,
  "roll_align",           "roll_align",          "Alignment of the rolling day window", TRUE,
  "water_year",           "water_year_start",    "Month defining start of the water year", TRUE,
  "years_range",          "start_year",          "Years to include in calculations", TRUE,
  "years_range",          "end_year",            "Years to include in calculations", TRUE,
  "years_exclude",        "exclude_years",       "Years to exclude from calculations", TRUE,
  "daterange",            "start_date",          "Range of dates to include", TRUE,
  "daterange",            "end_date",          "Range of dates to include", TRUE,
  "add_year",             "add_year",            "Add data from a given year to the plot", TRUE,
  "months",               "months",              "Months to include in calculations", TRUE,
  "percentiles",          "percentiles",         "Percentiles to add to calculations", TRUE,
  "plot_log",             "log_discharge",       "Plot data on log scale",            TRUE,
  "custom_months",        "custom_months",       "Months to combine and summarize as an additional row in the table", TRUE,
  "custom_months_label",  "custom_months_label", "What to label this range of months in the table", TRUE,
  "missing",              "ignore_missing",      "Whether dates with missing values should be included in calculations", TRUE,
  "allowed",              "allowed_missing",     "Percentage of data that can be missing", TRUE,
  "complete",             "complete_years",      "Whether to include only years with complete data in calculations", TRUE,
  "seasons",              "include_seasons",     "Whether or not to include seasonal calculations", TRUE,
  "mad",                  "percent_MAD",         "Mean annual discharge percentiles to add to the MAD table", TRUE,        #Unique
  "flow",                 "flow_value",          "Flow value from which to determine percentile rank", TRUE,               #Unique
  "percent",              "percent_total",       "Percentiles of total annual flows for which to dermine dates", TRUE,     #Unique
  "normal",               "normal_percentiles",  "Range of percentiles in which data are considered normal", TRUE,       #Limited
  "zyp",                  "zyp_method",          "Prewhitened trend method to use. zhang is recommended over yuepilon for hydrologic applications", TRUE,
  "alpha",                "zyp_alpha",           "Alpha to use for determining significance of trends", TRUE,
  "prob_plot", "prob_plot_position", "Type of calculation used to determine plotting positions in the frequency plot", TRUE,
  "prob_scale", "prob_scale_points", "Probabilities to be plotted along the X axis in the frequency plot", TRUE,
  "use_max", "use_max", "Use low or high flow values in analysis", TRUE,
  "use_log", "use_log", "Log10 transform data prior to analysis", TRUE,
  "fit_distr", "fit_distr", "Distribution used to fit annual data", TRUE,
  "fit_distr_method", "fit_distr_method", "Method used to fit the distribution.<br>Generally use MOM for PIII and MLE for Weibull", TRUE,
  "fit_quantiles", "fit_quantiles", "Quantiles to be estimated from the fitted distribution", TRUE,
  "plot_curve", "plot_curve", "Whether to add the computed curve to the probability plot", TRUE
) %>%
  group_by(id) %>%
  mutate(tooltip = if_else(add_arg,
                           paste0(tooltip, "<br>(<code>",
                                  glue_collapse(fasstr_arg, sep = ", "),
                                  "</code>)"),
                           tooltip))

tips <- as.list(parameters$tooltip) %>%
  unique() %>%
  setNames(unique(parameters$id))

usethis::use_data(parameters, tips, internal = TRUE, overwrite = TRUE)

# Many parameters in compute_annual_trends are already covered here,
# e.g., annual_percentiles, monthly_percentiles get the "percentiles" tooltip, etc.





# TO ADD ----- Functions to add from fasstr: ------------------
# - calc_longterm_monthly_stats
# - plot_longterm_monthly_stats
# - compute_frequency_quantile ??? only gives single value
# - compute_frequency_analysis (not necessary?)
# - calc_longterm_percentile - Deliberately not included (redundant with other tables)

# Arguments to add ------------------------

# calc_longterm_daily_stats
# - include_longterm (always included)

# plot_daily_stats
# - include_extremes
# - inner_percentiles
# - outer_percentiles

# plot_longterm_daily_stats
# - include_extremes
# - inner_percentiles
# - outer_percentiles

# plot_flow_duration
# - include_longterm

# plot_daily_cumulative_stats
# - basin_area

# plot_monthly_cumulative_stats
# - basin_area

# plot_annual_cumulative_stats
# - basin_area

# calc_daily_cumulative_stats
# - basin_area

# calc_monthly_cumulative_stats
# - basin_area

# calc_annual_cumulative_stats
# - basin_area

# calc_annual_trends
# - basin_area


# compute_annual_frequencies
# - fit_distr_method -> Deliberately not included, allow default use depending on fit distribution
