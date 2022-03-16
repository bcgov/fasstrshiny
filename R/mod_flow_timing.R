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

# Flow timing ------------------------------
ui_flow_timing <- function(id) {


  fluidRow(
    column(
      width = 12, h2("Flow Timing"),
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        div(style = "max-width: 300px;",
            selectizeInput("ft_percent",
                           label = "Percents of total annual flows",
                           choices = c(1:99),
                           selected = c(25, 33, 50, 75),
                           multiple = TRUE),
            bsTooltip("ft_percent", tips$percent, placement = "left")
        )),
      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          girafeOutput("ft_plot", height = plot_height)
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DTOutput("ft_table")
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("ft_code")
        )
      )
    )
  )

}
