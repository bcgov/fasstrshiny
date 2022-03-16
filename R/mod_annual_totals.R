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

# Annual Totals -------------------------------

ui_annual_totals <- function(id) {

  fluidRow(
    column(
      width = 12, h2("Annual Totals"),
      box(width = 3,
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          div(id = "ann_tot_seasons_tip",
              prettySwitch(
                "ann_tot_seasons",
                label = "Include seasons",
                value = formals("plot_annual_cumulative_stats")$include_seasons,
                status = "success", slim = TRUE)),
          bsTooltip("ann_tot_seasons_tip", tips$seasons, placement = "left")),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          girafeOutput("ann_tot_plot", height = plot_height)
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DTOutput("ann_tot_table")
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("ann_tot_code")
        )
      )
    )
  )
}
