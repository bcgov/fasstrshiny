# Code Design

This document records general code-design related decisions in an effort to 
simplify future development.

Much of the design of fasstrshiny is related to the design of fasstr, which has
a series of functions following similar naming conventions and with similar
arguments.

Most sections of fasstrshiny related to families of functions within fasstr.

## Organization
Each panel of the app has a corresponding section in both the server.R and the ui.R scripts.
Interactively built UI elements are at the *top* of the server.R script under "UI elements".
Non-interactively built UI elements are in their corresponding section in the ui.R script.
Functions for building commonly used inputs are in the global.R script. 
Functions used for generic, non-shiny specific, tasks are in utils.R script in the fasstrshiny R folder.

The ui.R script has each panel built as a separate `fluidRow()` which is then combined in the the UI at the bottom of the script under "Combine". 
In this section, the menu is created and the panels and tabs are assembled. 
Note that each section needs to be created in the menu, which then links to dashboard body, which includes the `fluidRow()` object. 

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
  
## Missing vs. Allowed missing
- If `allowed_missing` exists, use only that (it overrides `ignore_missing` anyway)
- If a tab had options for either (i.e. when there is `type`) then use the `miss_allowed` ui to switch between the two.

## `ggiraph`
- Always use `girafe`, `girafeOutput` and `renderGirafe` (not any of the ggiraph variantes)
- Always use `girafe(ggobj = PLOT)` (`ggobj` is the important argument here)
- When using the vline tooltip (`create_vline_interactive()`) you'll need to 
  adjust the `opts_hover()` option in the girafe output to make opacity 1. 

## IDs
See the internal dataset, `parameters`. It is created in `data-raw/parameters.R` 
and includes parameter `id`s, `tooltips` and how they correspond to fasstr arguments.

## Spinners
Spinners are created with the `shinycssloaders` package. The global options 
are set in `global.R`. Every output that requires a progress spinner needs to be 
wrapped with `withSpinner()` in `ui.R`.


## Future considerations

### Interactive Plots
Right now we use the static plots created by fasstr and then *add* interactivity 
to these plots with either ggiraph (adding/replacing interactive geoms) or 
with plotly (ggplotly). In the future, interactivity *could* be added to fasstr
plots, however, there is the risk of making it necessary to update the fasstr 
app when all you to do is tweak something in fasstrshiny. It would also 
require more dependencies in fasstr.
