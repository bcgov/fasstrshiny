## code to prepare `parameters` dataset goes here

parameters <- dplyr::tribble(
  ~id,                    ~fasstr_arg,           ~tooltip,                             ~add_arg,
  "stats",                "include_stats",       "Choose statistics to display",       TRUE,
  "availability",         "plot_availability",   "Indicate years which contain only partial data", TRUE,
  "symbols_percent",      "plot_percent",        "Plot days as proportion rather than number", TRUE,
  "add_dates",            "",                    "Choose dates to highlight on the plot",   FALSE,
  "add_mad",              "",                    "Add the calculated Mean Annual Discharge percentiles to the plot", FALSE,

  "discharge",            "values",              "Discharge to use in calculations<br>(Yield only available if basin area provided. See settings in Data > Loading)",   FALSE,
  "discharge2",           "use_yield",           "Discharge to use in calculations",   FALSE,
  "roll_days",            "roll_days",           "Number of days which to have a rolling average", TRUE,
  "roll_align",           "roll_align",          "Direction of rolling average window alignment (Right = last of n days, Left = first of n days, Center = middle of n days", TRUE,
  "longterm",             "include_longterm",    "Include long-term data in calculations", TRUE,
  "water_year",           "water_year_start",    "Month defining start of the water year", TRUE,
  "years_range",          "start_year",          "Years to include in calculations", TRUE,
  "years_range",          "end_year",            "Years to include in calculations", TRUE,
  "years_exclude",        "exclude_years",       "Years to exclude from calculations", TRUE,
  "daterange",            "start_date",          "Range of dates to include", TRUE,
  "daterange",            "end_date",            "Range of dates to include", TRUE,
  "basin_area",           "basin_area",          "Basin area in km<sup>2</sup>. Defaults to area specified by HYDAT stations data, but can be overriden here.<br>Must be specified for non-HYDAT data.", TRUE,
  "add_year",             "add_year",            "Add data from a given year to the plot", TRUE,
  "months",               "months",              "Months to include in calculations", TRUE,
  "percentiles",          "percentiles",         "Percentiles to add to calculations", TRUE,
  "inner_percentiles",    "inner_percentiles",   "Limits of inner percentile ribbon", TRUE,
  "outer_percentiles",    "outer_percentiles",   "Limits of outer percentile ribbon", TRUE,
  "normal_percentiles",   "normal_percentiles",  "Range of percentiles in which data are considered normal", TRUE,
  "plot_log",             "log_discharge",       "Plot data on log scale",            TRUE,
  "plot_extremes",        "include_extremes",    "Plot extreme values as min-max ribbon",            TRUE,
  "custom_months",        "custom_months",       "Months to combine and summarize as an additional row in the table", TRUE,
  "custom_months_label",  "custom_months_label", "What to label this range of months in the table", TRUE,
  "missing",              "ignore_missing",      "Whether dates with missing values should be included in calculations", TRUE,
  "allowed",              "allowed_missing",     "Percentage of data that can be missing", TRUE,
  "complete",             "complete_years",      "Whether to include only years with complete data in calculations", TRUE,
  "seasons",              "include_seasons",     "Whether or not to include seasonal calculations", TRUE,
  "mad",                  "percent_MAD",         "Percent of mean annual discharge to add to plot", TRUE,
  "flow",                 "flow_value",          "Flow value from which to determine percentile rank", TRUE,
  "percent",              "percent_total",       "Percentiles of total annual flows for which to determine dates", TRUE,
  "zyp",                  "zyp_method",          "Prewhitened trend method to use. zhang is recommended over yuepilon for hydrologic applications", TRUE,
  "annual_percentiles",   "annual_percentiles",  "", FALSE,  # use percentiles
  "monthly_percentiles",  "monthly_percentiles", "", FALSE,  # use percentiles
  "low_roll_days",        "lowflow_days",        "", FALSE, # use roll_days
  "low_roll_align",       "lowflow_align",       "", FALSE, # use roll_align
  "alpha",                "zyp_alpha",           "Alpha to use for determining significance of trends", TRUE,
  "allowed_annual",       "allowed_missing_annual", "", FALSE, # use allowed
  "allowed_monthly",      "allowed_missing_monthly", "", FALSE, # use allowed
  "timing_percent",       "timing_percent",      "", FALSE, # use percent

  "prob_plot", "prob_plot_position", "Type of calculation used to determine plotting positions in the frequency plot", TRUE,
  "prob_scale", "prob_scale_points", "Probabilities to be plotted along the X axis in the frequency plot", TRUE,
  "use_max", "use_max", "Use low or high flow values in analysis", TRUE,
  "use_log", "use_log", "Log10 transform data prior to analysis", TRUE,
  "fit_distr", "fit_distr", "Distribution used to fit annual data", TRUE,
  "fit_distr_method", "fit_distr_method", "Method used to fit the distribution.<br>Generally use MOM for PIII and MLE for Weibull", TRUE,
  "fit_quantiles", "fit_quantiles", "Quantiles to be estimated from the fitted distribution", TRUE,
  "plot_curve", "plot_curve", "Whether to add the computed curve to the probability plot", TRUE
) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(tooltip = dplyr::if_else(add_arg,
                                         paste0(tooltip, "<br>(<code>",
                                                glue::glue_collapse(fasstr_arg, sep = ", "),
                                                "</code>)"),
                                         tooltip))

tips <- parameters %>%
  dplyr::select(id, tooltip) %>%
  dplyr::distinct() %>%
  dplyr::pull(tooltip, id) %>%
  as.list()


opts <- list(
  plot_height = "500px",
  scale = 0.75
)

bc_maps_layers <- list(
  "hydrozones" = bcmaps::hydrozones(ask = FALSE),
  "nr_regions" = bcmaps::nr_regions(ask = FALSE),
  "nr_areas" = bcmaps::nr_areas(ask = FALSE),
  "ecoprovinces" = bcmaps::ecoprovinces(ask = FALSE),
  "wsc_drainages" = bcmaps::wsc_drainages(ask = FALSE)) %>%
  purrr::map(~sf::st_transform(., crs = 4326)) %>%
  purrr::map(~sf::st_simplify(., dTolerance = 2000))

bc_maps_labs <- dplyr::tribble(
  ~id, ~group, ~label,
  "hydrozones", "Hydrologic Zones",
  ~glue::glue("{stringr::str_to_title(HYDROLOGICZONE_NAME)}",
              "(No. {HYDROLOGICZONE_NO})"),
  "nr_regions", "Natural Resource Regions", ~stringr::str_to_title(REGION_NAME),
  "nr_areas", "Natural Resource Areas", ~stringr::str_to_title(AREA_NAME),
  "ecoprovinces", "Ecoprovinces", ~stringr::str_to_title(ECOPROVINCE_NAME),
  "wsc_drainages", "WSC Drainages",
  ~stringr::str_to_title(SUB_SUB_DRAINAGE_AREA_NAME))

mods <- c("data_load", "data_available", "hydro", "cumulative", "flows",
          "annual_stats", "annual_means", "annual_totals", "flow_timing",
          "low_flows", "high_flows", "outside_normal",
          "annual_trends", "volume_freq", "hydat_peak")

usethis::use_data(parameters, tips, opts, bc_maps_layers, bc_maps_labs, mods,
                  internal = TRUE, overwrite = TRUE)

# Many parameters in compute_annual_trends are already covered here,
# e.g., annual_percentiles, monthly_percentiles get the "percentiles" tooltip, etc.





# TO ADD ----- Functions to add from fasstr: ------------------
# - compute_frequency_quantile ??? only gives single value
# - compute_frequency_analysis (not necessary?)
# - calc_longterm_percentile - Deliberately not included (redundant with other tables)

# Arguments to add ------------------------

# calc_longterm_daily_stats
# - include_longterm (always included)

# plot_flow_duration
# - include_longterm

# compute_annual_frequencies
# - fit_distr_method -> Deliberately not included, allow default use depending on fit distribution
