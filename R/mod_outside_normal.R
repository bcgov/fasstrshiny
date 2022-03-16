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


# Days outside normal ------------------------
ui_outside_normal <- function(id) {
  fluidRow(
    column(
      width = 12, h2("Days Outside Normal"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        sliderInput("on_normal", label = "Normal range ",
                    value = c(25, 75), min = 1, max = 99, step = 1),
        bsTooltip("on_normal", tips$normal, placement = "left")
      ),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          girafeOutput("on_plot", height = plot_height)
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DTOutput("on_table")
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("on_code")
        )
      )
    )
  )
}
