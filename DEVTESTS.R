library(fasstr)
library(tidyverse)
library(tidyhydat)
library(patchwork)

d <- fill_missing_dates(station_number = "08HB048") %>%
  add_date_variables(water_year_start = 1) %>%
  add_daily_volume() %>%
  add_daily_yield()

plot_daily_stats(d, values = Yield_mm)


calc_longterm_mean(d, percent_MAD = c(5, 10 ,50), complete_years = TRUE)

plot_annual_cumulative_stats(d, include_seasons = TRUE) %>%
  wrap_plots(nrow = 2, byrow = FALSE, design = "AACC\nBBCC")

calc_annual_flow_timing(d)
plot_annual_flow_timing(d, values = Volume_m3)

calc_annual_lowflows(d)
plot_annual_lowflows(d, values = Volume_m3)[[1]] # Discharge
plot_annual_lowflows(d)[[2]] # DOY

calc_annual_peaks(d)

calc_annual_outside_normal(d)
plot_annual_outside_normal(d)


ft <- calc_annual_flow_timing(d)
at <- compute_annual_trends(d, zyp_method = "zhang")

ft
at$Annual_Trends_Data %>% filter(Statistic == "DoY_25pct_TotalQ")
at$Annual_Trends_Data

v <- compute_annual_frequencies(d, roll_days = c(1, 3))  # uses compute_frequency_analysis
names(v)

v$Freq_Analysis_Data
v$Freq_Plot_Data
v$Freq_Plot
v$Freq_Fitting

# Single value
compute_frequency_quantile(d, roll_days = 7, return_period = 10)

tidyhydat::download_hydat()
v2 <- compute_hydat_peak_frequencies("08HB048")

compute_hydat_peak_frequencies("08HB048", fit_distr = "PIII")
