# Code Design

This document records general code-design related decisions in an effort to 
simplify future development.

Much of the design of fasstrshiny is related to the design of fasstr, which has
a series of functions following similar naming conventions and with similar
arguments.

Most sections of fasstrshiny related to families of functions within fasstr.

## Organization
Each panel of the app has corresponding module functions in a file named
`mod_XXX.R`.
Interactively built UI elements are at the *top* of server functions.
Anything pertaining to modifying the UI (toggles, updates, bookmarking) are also 
in this section.

Non-interactively built UI elements are in their corresponding section in the ui function.
Functions for building commonly used inputs are in the helper_ui_inputs.R script. 
Other functions can be found in `helper_XXX.R` files.

The `fasster_shiny()` function combines all modules into a single app. 
Note that each section needs to be created in the menu, the UI tabsets AND the
server section.

## Namespace
Because modules create their own ids, we need to use the `NS()` function to
ensure that module ids are unique. In the UI functions, we use `ns <- NS(id)`
at the top of the function. Then we can use `ns("my_input")` throughout.

However, for server functions, we need to use `NS(id, "my_input")` directly.

For functions that create UI inputs, the `NS(id, ...)` is inside the function, 
so in the UI (or Server) function you would only pass the id: `select_rolling(id)`. 

## Inputs and IDs
Many inputs are the same among sections in fasstrshiny because they related to
arguments in fasstr which are common among function families.

To ensure consistency, functions (`select_XXXX()`) are used to create inputs for
each section. These functions create separate instances of the inputs, with
unique ids, but which are consistent.

For example, `select_rolling(id)` creates inputs `ID_roll_days`
(numeric input) and `ID_rolling_align` (select input).

Interactively built inputs need to be created in the server functions
under "UI elements" before they can be referenced in the ui with `uiOutput()`.

For IDs, see the internal dataset, `parameters`. It is created in `data-raw/parameters.R` 
and includes parameter `id`s, `tooltips` and how they correspond to fasstr arguments.

## Bookmarking
Bookmarking is URL-based on shinyapps.io and Server-based (i.e. saving a file to
the computer) on locally-run instances of `fasstrshiny`.

Bookmarking only applies to the full GUI run with `fasstr_shiny()`. It automatically
saves the state of all inputs. However, by default, bookmarking doesn't save the
state of *dynamically-created inputs*. These inputs must be saved by hand using
the `onBookmark()` function and then restored by hand using the `onRestored()` function.
The internal helper function `restore_inputs` defines how to restore the inputs.

If creating a new module with dynamic inputs, 

a) Make sure you have a section for saving and restoring these inputs 
   (see `mod_annual_trends.R` at the end of the "UI Elements" section for an example); 

b) Ensure the input ids *and* types exist in the `restore_inputs()` function, 
   if they do not, you'll have to add them (see documentation for 
   `restore_inputs()` in the `helper_shiny.R` file for details).
   
Note that not *every* dynamic input needs to be bookmarked. For example,
it is probably unnecessary to bookmark which plot is currently being displayed.
The default value is most likely sufficient.


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

For example, `create_fun()` takes the name of the fasstr function, the name of
the dataset, the shiny `input` object, and data settings. It then matches inputs
in that module against parameters in the fasstr function, omits those that have
default values, and creates a *text* version of the fasstr function with arguments.

Example from hydrograph figure:

```
data_flow <- data_raw()

g <- switch(input$type,
            "Daily" = "plot_daily_stats",
            "Long-term Monthly" = "plot_longterm_monthly_stats",
            "Long-term Daily" = "plot_longterm_daily_stats") %>%
  create_fun(data_name = "data_flow", input, input_data = data_settings(),
             extra = dplyr::if_else(
               input$add_year != "",
               glue::glue("add_year = {input$add_year}"),
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
then be parsed and evaluated with `eval_check(t)`, a function which evaluates
the text code and checks for errors.

**Adding new arguments**

New arguments can be added in one of two ways. If an argument is used in a
standard way, it's name/id can be added to the
`parameters` data frame created in `data-raw/parameters.R` and then how to use
it in a function can be added to the workflow in the `combine_parameters()`
function. As long as the input id is called `param`, id of the parameter in this
shiny app, it will be automatically used by `create_fun()`, unless added to the
`params_ignore` list.

Alternatively if an argument is used in a non-standard way, it
can be added with the `extra` argument. You'll also need to add it to 
`params_ignore` so as not to have two arguments in the final function.

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

NOTE: If adding a new *dynamic* input, ensure that it is captured by bookmarking.
(See Bookmarking above).

## Adding a new module

- Create new `mod_XXX.R` file with UI and server functions
- In `fasstr_shiny()`, 
  - add `ui_XXX()` function to the UI function, 
  - add reference to the sidebar function, 
  - add name (`XXX`) to the `mods` list in `data-raw/parameters.R`, re-run this file
- In the `server_XXX()` function, use `create_fun()` in the appropriate output
  or reactive
  - Add inputs for the values which are NOT set in data_settings() 
    (see bottom of `mod_data_load.R`)
  - `data_settings()` values will *automatically* be used in the function. If any
    should not (i.e. `discharge` is use to set `value` to a different flow type 
    column like yield or volume, but this isn't always appropriate), add
    the parameter to the argument `params_ignore`.
  - Add new arguments to 
    - the `parameters` list in `data-raw/parameters.R`
    - the `combine_parameters.R` function in `helper_create_fun.R`
  - New arguments that aren't standard (i.e. `percentiles` in `mod_hydro.R`)
    can be added via the `extra` argument
  - **If new inputs are created dynamically, ensure they are saved during bookmarking.** 
    Add new types of inputs to `restore_inputs()` in `helper_shiny.R` 
    (see `mod_hydro.R` for example). 
    If a compute button is required, ensure it is **NOT** bookmarked 
    (see `mod_annual_trends.R` for example)
- Add tests to `test_mod.R`, make sure every input gets a starting value


## Miscellaneous points

### Datatables

- With the scroller extension, must use the scrollY attribute to set table height
  (can't use pageLength)
  
### Missing vs. Allowed missing
- If `allowed_missing` exists, use only that (it overrides `ignore_missing` anyway)


### `ggiraph`
- Always use `girafe`, `girafeOutput` and `renderGirafe` (not any of the ggiraph variants)
- Always use `girafe(ggobj = PLOT)` (`ggobj` is the important argument here)
- When using the vline tooltip (`create_vline_interactive()`) you'll need to 
  adjust the `opts_hover()` option in the girafe output to make opacity 1. 

### Spinners
Spinners are created with the `shinycssloaders` package. The global options 
are set in `global.R`. Every output that requires a progress spinner needs to be 
wrapped with `withSpinner()` in `ui.R`.


## Troubleshooting
- Input/output doesn't render, no message, no error
  - Check to make sure id isn't duplicated
  - Check to make sure all the ids match up (i.e. same id, no spelling mistakes)

## Test errors
For a testthat error like:

`Failure (test_04d_modules_analysis.R:59:3): Hydat Peak
`output$table` threw an unexpected error.
Message: The test referenced an output that hasn't been defined yet: output$proxy1-table`

- Look at line 59 in the `test_04d_modules_analysis.R` file, this is the 
test that is failing. 

- "`output$table` threw an unexpected error" means that when testing `expect_error(output$table, NA)`
 (which means DON'T expect an error), there was an error
 
- "Message: The test referenced an output that hasn't been defined yet: output$proxy1-table`"
  means that the error returned refers to the fact that the `table` object hasn't 
  been rendered in the test Shiny server. This is why the test is failing.
  (Note that `proxy1-` is simply the id assigned to the namespace by `testServer()`, 
  the important part is the `table`, which tells you which object is causing problems).

- Check the following:
  - Is the output actually called `table`? (Was it changed? Is there a typo?)
  - Should the output actually be rendered by this point? Or does it need 
  another input, or a button click? If so, add that to `session$setInputs()`
  - Remember that *every* input needs to be defined in each test.

- You can add a `browser()` call inside the `testServer()` function if you need
 to do more thorough testing (see Mastering Shiny's section on Testing reactivitity <https://mastering-shiny.org/scaling-testing.html#testing-reactivity>)
    


## Tips
- Ctrl-click on a function will jump you to the code where the function is created.
  If it's not part of this package, it'll open an observer with information about the
  function as well as the package it's from
  
- `browser()` in code will automatically pause the code/app and let you use the 
  terminal. Great for testing shiny apps.
  
- https://mastering-shiny.org/


## Future considerations

### Interactive Plots
Right now we use the static plots created by fasstr and then *add* interactivity 
to these plots with either ggiraph (adding/replacing interactive geoms) or 
with plotly (ggplotly). In the future, interactivity *could* be added to fasstr
plots, however, there is the risk of making it necessary to update the fasstr 
app when all you to do is tweak something in fasstrshiny. It would also 
require more dependencies in fasstr.

