## code to prepare `parameters` dataset goes here
library(tidyverse)

parameters <- tribble(
  ~id,                    ~fasstr_arg,           ~tooltip,                             ~add_arg,
  "discharge",            "values / use_yield",  "Discharge to use in calculations",   FALSE,
  "roll_days",            "roll_days",           "Number of days over which to roll the mean", TRUE,
  "roll_align",           "roll_align",          "Alignment of the rolling day window", TRUE,
  "water_year",           "water_year_start",    "Month range defining the water year", TRUE,
  "years_range",          "start_year/end_year", "Years to include in calculations", TRUE,
  "years_exclude",        "exclude_years",       "Years to exclude from calculations", TRUE,
  "months",               "months",              "Months to include in calculations", TRUE,
  "percentiles",          "percentiles",         "Percentiles to add to calculations", TRUE,
  "plot_log",             "log_discharge",       "Plot data on log scale",            TRUE,
  "custom_months",        "custom_months",       "Months to combine and summarize as an additional row in the table", TRUE,
  "custom_months_label",  "custom_months_label", "What to label this range of months in the table", TRUE,
  "missing",              "ignore_missing",      "Whether dates with missing values should be included in calculations", TRUE,
  "allowed",              "allowed_missing",     "Percentage of data that can be missing", TRUE,
  "complete",             "complete_years",      "Whether to include only years with complete data in calculations", TRUE,
  "mad",                  "percent_MAD",         "Mean annual discharge percentiles to add to the MAD table", TRUE,        #Unique
  "flow",                 "flow_value",          "Flow value from which to determine percentile rank", TRUE,               #Unique
  "percent",              "percent_total",       "Percentiles of total annual flows for which to dermine dates", TRUE,     #Unique
  "ahlf_roll",            "roll_days",           "Multiple number of days over which to roll the mean", FALSE,             #Unique (ahlf)
  "normal",               "normal_percentiles",  "Range of percentiles in which data are considered 'normal'", TRUE,       #Limited
  "zyp",                  "zyp_method",          "Prewhitened trend method to use. 'zhang' is recommended over 'yuepilon' for hydrologic applications (Bürger 2017; Zhang and Zwiers 2004)", TRUE,
  "alpha",                "zyp_alpha",           "Alpha to use for determining significance of trends", TRUE,
  "prob_plot", "prob_plot_position", "Type of calculation used to determine plotting positions in the frequency plot", TRUE,
  "prob_scale", "prob_scale_points", "Probabilities to be plotted along the X axis in the frequency plot", TRUE,
  "use_max", "use_max", "Use low or high flow values in analysis", TRUE,
  "use_log", "use_log", "Log10 transform data prior to analysis", TRUE,
  "fit_distr", "fit_distr", "Distribution used to fit annual data", TRUE,
  "fit_quantiles", "fit_quantiles", "Quantiles to be estimated from the fitted distribution", TRUE,
  "plot_curve", "plot_curve", "Whether to add the computed curve to the probability plot", TRUE
)

# Many parameters in compute_annual_trends are already covered here,
# e.g., annual_percentiles, monthly_percentiles get the "percentiles" tooltip, etc.



usethis::use_data(parameters, overwrite = TRUE)

# TO ADD ----- Functions to add from fasstr: ------------------
# - calc_longterm_monthly_stats
# - plot_longterm_monthly_stats
# - plot_annual_means
# - compute_frequency_quantile ??? only gives single value
# - compute_frequency_analysis (not necessary?)

# - calc_longterm_percentile - Deliberately not included (redundant with other tables)

# Arguments to add ------------------------

# calc_longterm_mean
# - complete_years

# calc_daily_stats
# - complete_years

# calc_longterm_daily_stats
# - complete_years
# - include_longterm (always included)
# - custom months - what happens if months chosen are not sequential, is that an issue?

# plot_daily_stats
# - complete_years
# - include_extremes
# - inner_percentiles
# - outer_percentiles

# plot_longterm_daily_stats
# - complete_years
# - include_extremes
# - inner_percentiles
# - outer_percentiles

# plot_monthly_stats
# - percentiles

# plot_annual_stats
# - percentiles


# plot_flow_duration
# - include_longterm


# plot_daily_cumulative_stats
# - basin_area
# - add_year

# plot_monthly_cumulative_stats
# - basin_area
# - add_year

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
# - fit_distr_method -> Deliberately not included, allow default use dpendeing on fit distribution
