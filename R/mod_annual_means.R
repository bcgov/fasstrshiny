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

# Annual Means -----------------------
ui_annual_means <- function(id) {
  fluidRow(
    column(
      width = 12, h2("Annual Means"),
      tabBox(
        width = 12,
        #helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          girafeOutput("am_plot", height = plot_height)
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("am_code")
        )
      )
    )
  )
}
