# Code Design

This document records general code-design related decisions in an effort to 
simplify future development.

Much of the design of fasstrshiny is related to the design of fasstr, which has
a series of functions following similar naming conventions and with similar
arguments.

Most sections of fasstrshiny related to families of functions within fasstr.

## Inputs

Many inputs are the same among sections in fasstrshiny because they related to
arguments in fasstr which are common among function families.

To ensure consistency, functions (`select_XXXX()`) are used to create inputs for
each section. These functions create separate instances of the inputs, with
unique ids, but which are consistent.

For example, `select_rolling(id = "sum", input)` creates inputs `sum_roll_days`
(numeric input) and `sum_rolling_align` (select input) for the Summary
Statistics section.

To create a bunch of similar inputs, another function `build_ui()` can be used
by supplying the sames of all the inputs required. This can be used directly in
the ui, or with a renderUI() function in the server, depending on whether any
elements need to be dynamically created.

Interactively built inputs need to be created in the server under "UI elements" 
before they can be referenced in the ui with `uiOutput()`.

## Creating fasstr functions

fasstrshiny includes functions for assembling fasstr
functions based on user input in an effort to minimize the need for massive
if/else chains and to create a code record to share with the user (in "R Code"
tabs).

One of the main ideas in fasstrshiny is to share the code used to create the
output with the user. To ensure that the output is always consistent with the
code used in the app, all main fasstr functions are assembled as a text object
and then evaluated. The text version can be used in the R Code panels, and the
evaluated version used to create the shiny app outputs.

For example, create_fun() takes the name of the fasstr function, the name of the
dataset and the various fasstr function arguments which correspond to
fasstrshiny inputs to create a *text* version of a fastr function with
arguments:

Example from Summary Statistics Table:

```
flow_data <- data_raw()
t <- switch(input$sum_type,
                "Long-term" = "calc_longterm_daily_stats",
                "Annual" = "calc_annual_stats",
                "Monthly" = "calc_monthly_stats",
                "Daily" = "calc_daily_stats") %>%
      create_fun("flow_data", id = "sum", input,
                 params = c("discharge", "percentiles",
                            "roll_days", "roll_align",
                            "data_water_year",
                            "data_years_range", "data_years_exclude",
                            "months",
                            if_else(input$sum_type %in% c("Long-term", "Daily"),
                                    "missing", "allowed")))
```

Example *text* output:

```
calc_longterm_daily_stats(flow_data,
  values = "Value",
  percentiles = c(10, 90),
  roll_days = 1,
  roll_align = "right",
  months = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  ignore_missing = FALSE
)
```

This can be saved for sharing with the user through the R Code panel, and can 
then be parsed and evaluated with `eval(parse(text = t))`.


## Datatables

- With the scroller extension, must use the scrollY attribute to set table height
  (can't use pageLength)

## IDs

- `discharge`
- `percentiles`
- `custom_months`
- `custom_months_label`
- `months`
- `water_year`

- `missing`
- `complete`
