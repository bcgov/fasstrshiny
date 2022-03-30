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

# Peak flows ------------------------
ui_peak_flows <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Peak Flows"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        select_rolling(id)
      ),
      tabBox(
        width = 9,

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DT::DTOutput(ns("table"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_peak_flows <- function(id, data_settings, data_raw,
                              data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun(fun = "calc_annual_peaks", data_name = "data_flow",
                      input, input_data = data_settings())

      code$table <- t
      labels$table <- "Calculate Annual peak flows"

      eval_check(t) %>%
        prep_DT()
    })


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))
  })
}
