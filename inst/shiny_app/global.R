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

library(leaflet)
library(DT)
library(gt)
library(plotly)
library(patchwork)
library(ggiraph)

library(fasstr)
library(tidyhydat)
library(bcmaps)

source("functions.R")

# Get Tootips ---------------------------------
tips <- fasstrshiny:::tips

# tidyhydat Stations data -----------------------
stations_list <- hy_stn_data_range(prov_terr_state_loc = "BC") %>%
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

