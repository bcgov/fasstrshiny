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

prep_hydat <- function() {

  # tidyhydat Stations data -----------------------
  stations_list <- tidyhydat::hy_stn_data_range(prov_terr_state_loc = "BC") %>%
    dplyr::filter(DATA_TYPE == "Q") %>%
    dplyr::pull(STATION_NUMBER)


  ## Create a dataframe of all station metadata and a list of all stations
  stations <- tidyhydat::hy_stations(station_number = stations_list) %>%
    dplyr::left_join(tidyhydat::hy_agency_list(), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>%
    dplyr::rename("CONTRIBUTOR" = AGENCY_EN) %>%
    dplyr::left_join(tidyhydat::hy_agency_list(), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%
    dplyr::rename("OPERATOR" = AGENCY_EN) %>%
    dplyr::left_join(tidyhydat::hy_datum_list(), by = c("DATUM_ID" = "DATUM_ID")) %>%
    dplyr::rename("DATUM" = DATUM_EN) %>%
    dplyr::mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>%
    dplyr::left_join(tidyhydat::hy_reg_office_list(),
                     by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>%
    dplyr:: rename("REGIONAL_OFFICE" = REGIONAL_OFFICE_NAME_EN) %>%
    dplyr::left_join(tidyhydat::hy_stn_regulation(), by="STATION_NUMBER") %>%
    dplyr::select(STATION_NUMBER, STATION_NAME, PROV_TERR_STATE_LOC, HYD_STATUS,
                  LATITUDE, LONGITUDE, DRAINAGE_AREA_GROSS, RHBN,
                  REAL_TIME, REGULATED,CONTRIBUTOR, OPERATOR, REGIONAL_OFFICE, DATUM) %>%
    dplyr::mutate(RHBN = ifelse(RHBN, "YES", "NO"),
                  REAL_TIME = ifelse(REAL_TIME, "YES", "NO"),
                  REGULATED = ifelse(REGULATED, "YES", "NO"),
                  DRAINAGE_AREA_GROSS = round(DRAINAGE_AREA_GROSS, digits = 2))

  station_parameters <- tidyhydat::hy_stn_data_range() %>%
    dplyr::filter(DATA_TYPE == "Q"| DATA_TYPE == "H")  %>%
    dplyr::select(STATION_NUMBER, DATA_TYPE) %>% tidyr::spread(DATA_TYPE, DATA_TYPE) %>%
    dplyr::mutate(PARAMETERS = dplyr::case_when(is.na(H) ~ "FLOW",
                                                is.na(Q) ~ "LEVEL",
                                                TRUE ~ paste("FLOW AND LEVEL")))

  dplyr::left_join(stations,
                   dplyr::select(station_parameters, STATION_NUMBER, PARAMETERS),
                   by = "STATION_NUMBER") %>%
    dplyr::rename_all(stringr::str_to_lower) %>%
    dplyr::rename("province" = "prov_terr_state_loc") %>%
    dplyr::mutate(dplyr::across(c(-"station_number", -"province", -"latitude", -"longitude",
                                  "drainage_area_gross"), stringr::str_to_sentence))
}
