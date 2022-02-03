library(fasstr)
library(tidyverse)
library(tidyhydat)
library(patchwork)
library(ggiraph)

d <- fill_missing_dates(station_number = "08HB048") %>%
  add_date_variables(water_year_start = 1) %>%
  add_daily_volume() %>%
  add_daily_yield()

plot_annual_outside_normal(d, normal_percentiles = c(1,99))

calc_flow_percentile(d, flow_value = 10)
calc_longterm_percentile(d, percentiles = c(10, 90))

screen_data <- screen_flow_data(d)

g <- "ggplot(data = screen_data,
              aes(x = Year, y = Mean)) +
        theme_bw() +
        theme(axis.title = element_text(size = 15),
              plot.title = element_text(size = 15, hjust = 0.5),
              axis.text = element_text(size = 13)) +
        geom_line(colour = 'dodgerblue4') +
        geom_point(colour = 'firebrick3', size = 2)"

stat <- "Mean"
g <- parse(text = g) %>%
  eval()

g2 <- g + geom_point_interactive(aes(tooltip = paste0("Year: ", Year, "\n",
                                                      "Mean: ", .data[[stat]]),
                                     data_id = Year),
                                 colour = 'firebrick3', size = 2)
girafe(ggobj = g2)


g <- plot_missing_dates(d)[[1]]
g$layers[[1]] <- geom_bar_interactive(
  aes(tooltip = paste0("Year: ", Year, "\nMissing: ", Value),
      data_id = paste0(Year, "-", Month)), colour = "cornflowerblue",
  fill = "cornflowerblue", stat = "identity")

girafe(ggobj = g)





plot_flow_data(d)[[1]] %>%
  to_ggiraph() %>%
  ggiraph(ggobj = .)


dts <- as.Date(c("1900-06-30", "1900-07-30", "1900-04-30"))
plot_daily_stats(d, values = Yield_mm, ignore_missing = TRUE, add_year = 2000, log_discharge = TRUE)[[1]] +
  geom_vline(xintercept = dts, colour = "grey20") +
  annotate(geom = "text", x = dts, y = Inf, vjust = 2, hjust = 1.05, label = dts)


plot_longterm_daily_stats(d, values = Yield_mm, ignore_missing = TRUE, add_year = 2000, log_discharge = TRUE)[[1]] +
  geom_vline(xintercept = dts, colour = "grey20") +
  annotate(geom = "text", x = dts, y = Inf, vjust = 2, hjust = 1.05, label = dts)

plot_daily_stats(d, values = Yield_mm)[[1]]$mapping


mad <- calc_longterm_mean(d, percent_MAD = c(5, 10, 50, 99)) %>%
  pivot_longer(-STATION_NUMBER, names_to = "type")

plot_daily_stats(d, values = Yield_mm, ignore_missing = TRUE, add_year = 2000, log_discharge = TRUE)[[1]] +
  geom_hline(yintercept = mad$value, size = c(1, rep(0.5, nrow(mad) - 1))) +
  geom_text(data = mad, aes(y = value, label = type),
            x = c(Inf, rep(-Inf, nrow(mad) - 1)),
            hjust = c(1.1, rep(-0.1, nrow(mad) -1)), vjust = -0.5)

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

# Annual trends
at <- compute_annual_trends(d, zyp_method = "zhang")

to_ggiraph(at[[4]], type = "Annual Mean")

at$Annual_Trends_Data %>% filter(Statistic == "DoY_25pct_TotalQ")
at$Annual_Trends_Data


# Vol freq
v <- compute_annual_frequencies(d, roll_days = c(1, 3, 7, 9))  # uses compute_frequency_analysis
names(v)

v$Freq_Analysis_Data
v$Freq_Plot_Data
v$Freq_Plot
v$Freq_Fitting
v$Freq_Fitted_Quantiles

to_ggiraph(v$Freq_Plot, type = "flow")


# Single value
compute_frequency_quantile(d, roll_days = 7, return_period = 10)

tidyhydat::download_hydat()
v2 <- compute_hydat_peak_frequencies("08HB048")

compute_hydat_peak_frequencies("08HB048", fit_distr = "PIII")



# Just in case
# ggplotly_cust <- function(x, digits = 4) {
#   opts <- options(digits = digits)
#   p <- plotly::ggplotly(x) %>%
#     plotly::config(modeBarButtons = list(list("toImage")))
#   options(opts)
#   p
# }
