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

library(glue)
library(dplyr)
library(tidyr)
library(stringr)

library(leaflet)
library(DT)
library(plotly)

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



# Functions ---------------------

## Functions for inputs ------------
select_months <- function(id, input, set = TRUE) {
  renderUI({
    if(set) {
      selected <- input$opts_months
      value <- input$opts_months_label
    } else {
      selected <- NULL
      value <- NULL
    }

    tagList(
      selectInput(paste0(id, "_months"),
                  label = "Custom Months",
                  choices = list("Jan" = 1,  "Feb" = 2,
                                 "Mar" = 3,  "Apr" = 4,
                                 "May" = 5,  "Jun" = 6,
                                 "Jul" = 7,  "Aug" = 8,
                                 "Sep" = 9,  "Oct" = 10,
                                 "Nov" = 11, "Dec" = 12),
                  selected = selected,
                  multiple = TRUE),

      textInput(paste0(id, "_months_label"),
                label = "Custom Months Label",
                placeholder = "ex. Jun-Aug",
                value = value)
    )
  })
}

select_discharge <- function(id, input, set = TRUE) {
  renderUI({
    if(set) selected <- input$opts_discharge else selected <- NULL
    radioButtons(paste0(id, "_discharge"),
                 label = "Discharge type",
                 choices = list("Discharge (cms)" = 1,
                                "Volumetric Discharge (m3)" = 2,
                                "Runoff Yield (mm)" = 3),
                 selected = selected)
  })
}

select_rolling <- function(id, input, set = TRUE) {
  renderUI({
    if(set) {
      value <- input$opts_roll_days
      selected <- input$opts_roll_align
    } else {
      value <- 1
      selected <- NULL
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
  })
}

select_percentiles <- function(id, input, set = TRUE) {
  renderUI({
    if(set) selected <- input$opts_percentiles else selected <- c(10,90)
    selectInput(paste0(id, "_percentiles"),
                label = "Percentiles to calculate",
                choices = c(1:99),
                selected = selected,
                multiple = TRUE)
  })
}

select_missing <- function(id, input, set = TRUE) {
  renderUI({
    if(set) value <- input$opts_missing else value <- FALSE
    checkboxInput(paste0(id, "_missing"),
                  label = "Ignore missing values",
                  value = value)
  })
}

select_extra <- function(id) {
  renderUI({
    materialSwitch(
      inputId = glue("{id}_show_extra"),
      label = "Show/hide extra options",
      value = FALSE,
      status = "success"
    )
  })
}

select_plot_options <- function(data, id, input) {
  div(align = "right",
      dropdownButton(
        tags$h3("Plot options"),
        materialSwitch(glue("{id}_log"), label = "Use log scale", value = FALSE,
                       status = "success"),
        dateRangeInput(glue("{id}_date_range"), "Start/End dates",
                       format = "yyyy-mm-dd", startview = "month",
                       start = min(data$Date), end = max(data$Date)),
        select_discharge(id, input),
        status = "primary", icon = icon("gear", verify_fa = FALSE),
        size = "sm", width = "300px", right = TRUE,
        tooltip = tooltipOptions(title = "Plot options", placement = "left")
      )
  )
}


ui_hidden <- c("rolling", "months", "percentiles", "missing")

build_ui <- function(id, input, define_options = FALSE, include) {

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
    ui_show <- include[!include %in% ui_hidden] %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList) %>%
      append(tagList(select_extra(id)))

    ui_hide <- include[include %in% ui_hidden] %>%
      purrr::map(~get(paste0("select_", .))(id, input, set)) %>%
      purrr::map(tagList)

    ui <- tagList(ui_show,
                  conditionalPanel(glue("input.{id}_show_extra == true"),
                                   ui_hide))
  }

  renderUI(ui)
}


## Functions for plots ------

plot_timeseries <- function(data, id, input){
  req(!is.null(input[[glue("{id}_log")]]),
      input[[glue("{id}_date_range")]],
      input$data_years_range,
      input$data_water_year)

  suppressWarnings({
    plot_flow_data(data = data,
                   # Global data options
                   water_year_start = as.numeric(input$data_water_year),
                   start_year = input$data_years_range[1],
                   end_year = input$data_years_range[2],
                   exclude_years = as.numeric(input$data_years_exclude),
                   # Plot specific options
                   log_discharge = input[[glue("{id}_log")]],
                   start_date = input[[glue("{id}_date_range")]][1],
                   end_date = input[[glue("{id}_date_range")]][2])[[1]] +
      scale_color_manual(values = "dodgerblue4")
  })
}


## Functions for inputs ---------
input_defaults <- function(id, default) {
  reactive({
    if(is.null(input[[id]])) default else input[[id]]
  })
}

code_format <- function(expr) {
  expr %>%
    format() %>%
    str_remove_all("^\\{|\\}$") %>%
    str_squish() %>%
    str_replace_all("%>%", "%>%\n ")
}
