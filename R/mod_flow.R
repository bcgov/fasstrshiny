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


# Flow ----------------
ui_flows <- function(id) {

  fluidRow(
    column(
      width = 12, h2("Flow duration and percentiles"),

      ## Settings -----------------------------
      box(
        width = 3,
        helpText("Placeholder descriptive text to describe this section, ",
                 "what it does and how to use it"),
        #numericInput("flows_flow",
        #             label = "Flow value for percentile",
        #             value = 10, min = 0),
        #bsTooltip("flows_flow", tips$flow, placement = "left"),
        checkboxGroupButtons(
          "flows_months",
          label = "Months to plot",
          choices = list("Jan" = 1, "Feb" = 2,
                         "Mar" = 3, "Apr" = 4,
                         "May" = 5, "Jun" = 6,
                         "Jul" = 7, "Aug" = 8,
                         "Sep" = 9, "Oct" = 10,
                         "Nov" = 11, "Dec" = 12),
          selected = c(1:12)),
        bsTooltip("flows_months", "Months to include/exclude from the plot",
                  placement = "left"),
        select_custom_months("flows"),

        # Update button
        bsButton("flows_compute", "Update", style = "primary",
                 class = "centreButton", ),
      ),
      tabBox(
        width = 9,

        ## Plot ---------------------
        tabPanel(
          title = "Plot - Flow duration",
          uiOutput("ui_flows_plot_options", align = "right"),
          withSpinner(girafeOutput("flows_plot", height = plot_height))
          #p(style = "margin-bottom: 20px"),
          #h4("Percentile Rank of Flow"),
          #textOutput("flows_perc")
        ),

        ## Table ---------------------
        tabPanel(
          title = "Table - Percentiles",
          select_table_options("flows", include = "custom_months"),
          withSpinner(DTOutput("flows_table"))
        ),

        ## R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("flows_code")
        )
      )
    )
  )
}
