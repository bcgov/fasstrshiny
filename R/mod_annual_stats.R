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


# Annual Statistics ------------------------------------------------
ui_annual_stats <- function(id) {

  fluidRow(
    column(
      width = 12, h2("Annual Statistics"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        div(align = "left",
            awesomeRadio("as_type",
                         label = "Summary type",
                         choices = list("Monthly",
                                        "Annual"),
                         selected = "Monthly",
                         status = "primary")),
        bsTooltip("as_type", "Type of statistic to calculate", placement = "left")
      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot",
          uiOutput("ui_as_plot_options", align = "right"),
          girafeOutput("as_plot", height = plot_height)
        ),

        ## Table ---------------------
        tabPanel(
          title = "Table",
          select_table_options("as", include = "percentiles"),
          DTOutput("as_table")
        ),

        ## R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("as_code")
        )
      )
    )
  )
}
