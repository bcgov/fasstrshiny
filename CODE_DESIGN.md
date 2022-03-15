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

For example, `create_fun()` takes the name of the fasstr function, the name of the
dataset, an id, and the shiny input object. It then matches inputs with that id
against parameters in the fasstr function (omitting those that are defaults), 
and creates a *text* version of the fasstr function with arguments.

Example from hydrograph figure:

```
flow_data <- data_raw()
g <- switch(input$hydro_type,
            "Daily" = "plot_daily_stats",
            "Long-term Monthly" = "plot_longterm_monthly_stats",
            "Long-term Daily" = "plot_longterm_daily_stats") %>%
    create_fun("data_flow", id = "hydro", input,
               extra = if_else(input$hydro_add_year != "",
                               glue("add_year = {input$hydro_add_year}"),
                               ""))
```

Example *text* output:

```
plot_daily_stats(data_flow,
  values = "Value",
  start_year = 1972,
  end_year = 2019,
  ignore_missing = TRUE,
  add_year = 2013
)
```

This can be saved for sharing with the user through the R Code panel, and can 
then be parsed and evaluated with `eval(parse(text = t))`.

**Adding new arguments**

New arguments can be added in one of two ways. If an argument is used in a
standard way in more than one function, it's name/id can be added to the
`parameters` data frame created in `data-raw/parameters.R` and then how to use
it in a function can be added to the workflow in the `combine_parameters()`
function. As long as the input is labelled `id_param` where id is the id of the
shiny group, and param is the id of the parameter in this shiny app, it will be
automatically used by `create_fun()`, unless added to the `params_ignore` list.

Alternatively if an argument is only used once or used in a non-standard way, it
can be added with the `extra` argument as in the example above. Note that if the
argument exists in `parameters` and in the `combine_parameters()` workflow, but
you would like to use it in a non-standard way with the `extra` argument, you'll
also need to add it to `params_ignore` so as not to have two arguments.

For example, in the Hydrographs table, we use percentiles from several inputs
(previously combined into `perc` in this example), so override the default usage
of the percentiles argument.

```
t <- switch(input$hydro_type,
            "Long-term Daily" = "calc_longterm_daily_stats",
            "Long-term Monthly" = "calc_longterm_monthly_stats",
            "Daily" = "calc_daily_stats") %>%
  create_fun(
    "data_flow", id = "hydro", input,
    # Because input$hydro_percentiles exists, but we don't want to use it
    params_ignore = "percentiles",
    extra = glue("percentiles = c({glue_collapse(perc, sep = ', ')})"))
```


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

## Troubleshooting
- Input/output doesn't render, no message, no error
  - Check to make sure id isn't duplicated
- Changes to functions.R/global.R don't show in the app
  - Make sure you completely restart the app (don't just refresh)


## Future considerations

### Interactive Plots
Right now we use the static plots created by fasstr and then *add* interactivity 
to these plots with either ggiraph (adding/replacing interactive geoms) or 
with plotly (ggplotly). In the future, interactivity *could* be added to fasstr
plots, however, there is the risk of making it necessary to update the fasstr 
app when all you to do is tweak something in fasstrshiny. It would also 
require more dependencies in fasstr.
