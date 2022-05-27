# Copyright 2022 Province of British Columbia
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

# Overview ---------------------------------------------------------------
ui_overview <- function(id) {

  fluidRow(
    column(
      width = 12, h2("Overview"),
      # box(width = 3,
      #    helpText("Placeholder descriptive text to describe this section, ",
      #              "what it does and how to use it")),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Overview",
          "map, HYDAT information",
          "n years of data, n missing, start, end, etcs",
          "basic month hydrograph - month means, with LTMADs",
          "annual daily hydrograph",
          "ltmad and 5, 10, 20 percent mads - with options",
          "30Q20 and freq - with options for return period/duration",
          "all time low, all time high")
      )
    )
  )
}

server_overview <- function(id, data_settings, data_raw,
                            data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

  })

}
