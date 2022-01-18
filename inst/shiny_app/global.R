# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)

library(glue)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(styler)

library(leaflet)
library(DT)
library(gt)
library(plotly)
library(patchwork)

library(fasstr)
library(tidyhydat)
library(bcmaps)


# tidyhydat Stations data -----------------------
stations_list <- tidyhydat::hy_stn_data_range(prov_terr_state_loc = "BC") %>%
  filter(DATA_TYPE == "Q") %>%
  pull(STATION_NUMBER)


## Create a dataframe of all station metadata and a list of all stations
stations <- hy_stations(station_number = stations_list) %>%  #c("AB","BC","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")
  left_join(hy_agency_list(), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>%
  rename("CONTRIBUTOR" = AGENCY_EN) %>%
  left_join(hy_agency_list(), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%
  rename("OPERATOR" = AGENCY_EN) %>%
  left_join(hy_datum_list(), by = c("DATUM_ID" = "DATUM_ID")) %>%
  rename("DATUM" = DATUM_EN) %>%
  mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>%
  left_join(hy_reg_office_list(),
            by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>%
  rename("REGIONAL_OFFICE" = REGIONAL_OFFICE_NAME_EN) %>%
  left_join(hy_stn_regulation(), by="STATION_NUMBER") %>%
  select(STATION_NUMBER, STATION_NAME, PROV_TERR_STATE_LOC, HYD_STATUS,
         LATITUDE, LONGITUDE, DRAINAGE_AREA_GROSS, RHBN,
         REAL_TIME, REGULATED,CONTRIBUTOR, OPERATOR, REGIONAL_OFFICE, DATUM) %>%
  mutate(RHBN = ifelse(RHBN, "YES", "NO"),
         REAL_TIME = ifelse(REAL_TIME, "YES", "NO"),
         REGULATED = ifelse(REGULATED, "YES", "NO"),
         DRAINAGE_AREA_GROSS = round(DRAINAGE_AREA_GROSS, digits = 2))

station_parameters <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q"| DATA_TYPE == "H")  %>%
  select(STATION_NUMBER, DATA_TYPE) %>% spread(DATA_TYPE, DATA_TYPE) %>%
  mutate(PARAMETERS = case_when(is.na(H) ~ "FLOW",
                                is.na(Q) ~ "LEVEL",
                                TRUE ~ paste("FLOW AND LEVEL")))

stations <- left_join(stations,
                      select(station_parameters, STATION_NUMBER, PARAMETERS),
                      by = "STATION_NUMBER") %>%
  rename_all(str_to_lower) %>%
  rename("province" = "prov_terr_state_loc") %>%
  mutate(across(c(-"station_number", -"province", -"latitude", -"longitude",
                  "drainage_area_gross"), str_to_sentence))

# bcmaps data -------------------
bc_hydrozones <- hydrozones(ask = FALSE) %>%
  sf::st_transform(crs = 4326)

# Settings --------------------------
min_height <- "250px" # Minimum placeholder height for boxes (will expand to content)
global_settings <- c("rolling", "months",                          # Settings tab
                     "years_range", "years_exclude", "water_year") # Data tab

# Functions ---------------------

## Functions for inputs ------------
select_months <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) {
    selected <- input$opts_months
    value <- input$opts_months_label
  } else {
    selected <- 1:12
    value <- ""
  }

  tagList(
    selectInput(paste0(id, "_months"),
                label = "Months to Include",
                choices = list("Jan" = 1,  "Feb" = 2,
                               "Mar" = 3,  "Apr" = 4,
                               "May" = 5,  "Jun" = 6,
                               "Jul" = 7,  "Aug" = 8,
                               "Sep" = 9,  "Oct" = 10,
                               "Nov" = 11, "Dec" = 12),
                selected = selected,
                multiple = TRUE)
  )
}

select_custom_months <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) {
    selected <- input$opts_custom_months
    value <- input$opts_custom_months_label
  } else {
    selected <- NULL
    value <- ""
  }

  tagList(
    selectInput(paste0(id, "_custom_months"),
                label = "Months to combine and summarize",
                choices = list("Jan" = 1,  "Feb" = 2,
                               "Mar" = 3,  "Apr" = 4,
                               "May" = 5,  "Jun" = 6,
                               "Jul" = 7,  "Aug" = 8,
                               "Sep" = 9,  "Oct" = 10,
                               "Nov" = 11, "Dec" = 12),
                selected = selected,
                multiple = TRUE),

    textInput(paste0(id, "_custom_months_label"),
              label = "Summary months label",
              placeholder = "ex. Jun-Aug",
              value = value)
  )
}

select_discharge <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) selected <- input$opts_discharge else selected <- NULL
  radioButtons(paste0(id, "_discharge"),
               label = "Discharge type",
               choices = list("Discharge (cms)" = "Value",
                              "Volumetric Discharge (m3)" = "Volume_m3",
                              "Runoff Yield (mm)" = "Yield_mm"),
               selected = selected)
}

select_rolling <- function(id, input = NULL, set = TRUE) {
    if(set & !is.null(input)) {
      value <- input$opts_roll_days
      selected <- input$opts_roll_align
    } else {
      value <- 1
      selected <- "right"
    }

    fluidRow(
      column(6,
             numericInput(paste0(id, "_roll_days"),
                          label = "Rolling avg. days",
                          value = value, min = 1, max = 180, step = 1)),
      column(6,
             selectInput(paste0(id, "_roll_align"),
                         label = "Rolling align",
                         selected = selected,
                         choices = list("Right" = "right",
                                        "Left" = "left",
                                        "Center" = "center")))
      )
}


select_percentiles <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) {
    selected <- input$opts_percentiles
  } else selected <- c(10,90)

  selectInput(paste0(id, "_percentiles"),
              label = "Percentiles to calculate",
              choices = c(1:99),
              selected = selected,
              multiple = TRUE)
}

select_complete <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) value <- input$opts_complete else value <- FALSE
  checkboxInput(paste0(id, "_complete"),
                label = "Complete years only")
}

select_missing <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) value <- input$opts_missing else value <- FALSE
  checkboxInput(paste0(id, "_missing"),
                label = "Ignore missing values")
}

select_allowed <- function(id, input = NULL, set = TRUE) {
  if(set & !is.null(input)) value <- input$opts_allowed else value <- FALSE
  sliderInput(paste0(id, "_allowed"),
              label = "Allowed missing (%)",
              value = value, step = 5, min = 0, max =100)
}

select_extra <- function(id) {
  materialSwitch(
    inputId = glue("{id}_show_extra"),
    label = "Show/hide extra options",
    value = FALSE,
    status = "success"
  )
}

select_parameters <- function(id, params) {
  checkboxGroupButtons(glue("{id}_params"),
                       label = "Statistics",
                       choices = params,
                       selected = params)
}

select_plot_options <- function(id, input,
                                include = "log",
                                params = NULL, data = NULL) {

  i <- tagList()
  if("log" %in% include) {
    i <- tagList(i, materialSwitch(glue("{id}_log"),
                                   label = "Use log scale", value = FALSE,
                                   status = "success"))
  }
  if("daterange" %in% include) {
    if(is.null(data)) stop("Require 'data' to create daterange UI",
                           call. = FALSE)

    i <- tagList(i, dateRangeInput(glue("{id}_daterange"), "Start/End dates",
                                   format = "yyyy-mm-dd", startview = "month",
                                   start = min(data$Date), end = max(data$Date)))
  }
  if("discharge" %in% include) {
    i <- tagList(i, select_discharge(id, input))
  }
  if("parameters" %in% include) {
    if(is.null(params)) stop("Require 'params' to create parameters UI",
                           call. = FALSE)

    i <- tagList(i, select_parameters(id, params))
  }

  t <- tagList(
    div(align = "right",
        dropdownButton(
          tags$h3("Plot options"),
          i,
          status = "primary", icon = icon("gear", verify_fa = FALSE),
          size = "sm", width = "300px", right = TRUE,
          tooltip = tooltipOptions(title = "Plot options", placement = "left")
        )
    )
  )
}

select_table_options <- function(id, input,
                                 include = c("percentiles", "custom_months"),
                                 params = NULL, data = NULL) {

  i <- tagList()
  if("percentiles" %in% include) {
   i <- tagList(i, select_percentiles(id, input, set = TRUE))
  }
  if("custom_months" %in% include) {
    i <- tagList(i, select_custom_months(id, input, set = TRUE))
  }

  t <- tagList(
    div(align = "right",
        dropdownButton(
          tags$h3("Table options"),
          i,
          status = "primary", icon = icon("gear", verify_fa = FALSE),
          size = "sm", width = "300px", right = TRUE,
          tooltip = tooltipOptions(title = "Table options", placement = "left")
        )
    )
  )
}



# Disable/Enable 'allowed'
toggle_allowed <- function(id, input) {
  observe({
    req(!is.null(input[[glue("{id}_missing")]]),
        input[[glue("{id}_type")]])
    if(input[[glue("{id}_missing")]] ||
       input[[glue("{id}_type")]] %in% c("Long-term", "Daily")) {
      disable(glue("{id}_allowed"))
    } else enable(glue("{id}_allowed"))
  })
}


build_ui <- function(id, input = NULL, define_options = FALSE,
                     include, hide = c("allowed", "custom_months", "percentiles"),
                     global = global_settings) {

  if(any(global %in% include) & !define_options){
    stop("Some ui elements included here (",
         paste0(include[include %in% c("rolling", "months")],
                collapse = ", "),
         ") should only be set once on the 'Settings' tab.\n",
         "They should not be set on individual tabs.",
         call. = FALSE)
  }

  # Set up all options in the settings
  # - Don't hide and don't set defaults
  if(define_options == TRUE) {
    set <- FALSE
    ui <- include %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList)
  } else {
  # Set up options for a specific tab
  # - Hide extra options
  # - Set defaults from the Settings tab
    set <- TRUE
    ui <- include[!include %in% hide] %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList) %>%
      append(tagList(select_extra(id)))

    ui_hide <- include[include %in% hide] %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList)

    if(length(include[include %in% hide]) > 0) {
      ui <- tagList(ui,
                    conditionalPanel(glue("input.{id}_show_extra == true"),
                                     ui_hide))
    }
  }
  ui
}


#' Combine elements for functions
#'
#' @param fun Character. Name of the fasstr function to use
#' @param data Character. Name of the data to use
#' @param id Character. Input/output id (e.g., "sum")
#' @param input Shiny input object
#' @param params Character vector of inputs to become arguments. Unnamed inputs
#'   are assumed to be "id_input". Any input not part of id can be named (e.g.,
#'   "data" = "water_year").
#' @param extra Character. String with extra arguments not related to shiny
#'   inputs
#' @param end Character. String to put after the function (e.g., "[[1]]" for
#'   plots)
#'
#' @return
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' t <- create_fun(fun = "calc_longterm_daily_stats",
#'                 data = "flow_data", id = "sumfl", input,
#'                 params = c("discharge", "roll_days", "roll_align",
#'                            "data" = "water_year",
#'                            "data" = "years_range",
#'                            "data" = "years_exclude",
#'                            "months", "missing"))
#'
#'
#' t <- create_fun(fun = "calc_longterm_mean",
#'                 data = "flow_data", id = "sumsi", input,
#'                 params = c("discharge", "roll_days", "roll_align",
#'                            "water_year", "years_range", "years_exclude",
#'                            "months", "mad"),
#'                params_extra = c("mad" = "percent_MAD = c(input$sumsi_mad)"))
#' }

create_fun <- function(fun, data, id, input, params, params_ignore = NULL,
                       extra = NULL, end = "") {

  params_default <- c("discharge", "roll_days", "roll_align", "water_year",
                      "years_range", "years_exclude", "months")

  if(!is.null(params_ignore)) {
    params_default <- params_default[!params_default %in% params_ignore]
  }

  params <- unique(c(params, params_default))

  # Figure out where parameters come from
  if(is.null(names(params))) {
    n <- rep(NA_character_, length(params))
  } else n <- names(params)
  n[params %in% c("roll_days", "roll_align", "months")] <- "opts"
  n[params %in% c("water_year", "years_range", "years_exclude")] <- "data"
  n[is.na(n)] <- id
  names(params) <- glue("{n}_{params}")

  # Retrieve inputs for these parameters
  id <- map(names(params), ~input[[.]])

  # Remove NULL/empty
  nulls <- map_lgl(id, ~is.null(.) || (is.character(.) && . == ""))
  id <- id[!nulls]
  params <- params[!nulls]

  # Create standard parameters
  #
  # - REMEMBER! When collapsing multiple elements with glue_collapse, use [[i]]
  p <- vector()
  for(i in seq_along(params)) {
    p[i] <- case_when(

      # Specific
      params[i] == "discharge" ~ glue("values = '{id[i]}'"),
      params[i] == "percentiles" ~
        glue("percentiles = c({glue_collapse(id[[i]], sep = ', ')})"),
      params[i] == "custom_months" ~
        glue("custom_months = c({glue_collapse(id[[i]], sep = ', ')})"),
      params[i] == "custom_months_label" ~ glue("custom_months_label = '{id[i]}'"),
      params[i] == "missing" ~ glue("ignore_missing = {id[i]}"),
      params[i] == "allowed" ~ glue("allowed_missing = {id[i]}"),
      params[i] == "complete" ~ glue("complete_years = {id[i]}"),

      # Opts
      params[i] == "roll_days" ~ glue("roll_days = {id[i]}"),
      params[i] == "roll_align" ~ glue("roll_align = '{id[i]}'"),
      params[i] == "months" ~
        glue("months = c({glue_collapse(id[[i]], sep = ', ')})"),

      # Data
      params[i] == "water_year" ~
        glue("water_year_start = {id[i]}"),
      params[i] == "years_range" ~
        glue("start_year = {id[[i]][1]}, end_year = {id[[i]][2]}"),
      params[i] == "years_exclude" ~
        glue("exclude_years = {id[i]}"),

      # Plot
      params[i] == "daterange" ~
        glue("start_date = '{id[[i]][1]}', end_date = '{id[[i]][2]}'"),
      params[i] == "log" ~ glue("log_discharge = {id[i]}")
      )
  }


  args <- glue_collapse(c(data, p, extra), sep = ', ')

  glue("{fun}({args}){end}")
}



## Check Functions ---------

## Other Functions ---------

code_format <- function(code, id) {
  str_subset(names(code), glue("{id}_")) %>%
    sort() %>%
    map(~code[[.]]) %>%
    as.character() %>%
    str_remove_all("^\\{|\\}$") %>%
    str_squish() %>%
    glue_collapse("\n\n") %>%
    str_replace_all("%>%", "%>%\n ") %>%
    str_replace_all("\\+", "\\+\n ") %>%
    str_replace_all("&&", "\n\n") %>%
    code_break_lines()
}

code_break_lines <- function(code) {
  s <- str_split(code, "\n") %>% unlist()
  l <- map_lgl(s, ~nchar(.) > 80)
  # only set new line if list items are longer than 4 and not near the end of a line
  s[l] <- str_replace_all(s[l], ",(?! [:print:]{1,4}(,|[:punct:]{1,4}$))", ",\n")

  glue_collapse(s, "\n") %>%
    style_text() %>%
    as.character() %>%
    glue_collapse("\n") %>%
    str_remove_all("^\n*")
}




