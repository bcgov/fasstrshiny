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

# Annual Trends -----------------------
ui_annual_trends <- function(id) {
  fluidRow(
    column(
      width = 12, h2("Annual Trends"),
      box(width = 3,

          # Compute button
          bsButton("at_compute", "Compute Trends", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          hr(class = "narrowHr"),

          # Other options
          uiOutput("ui_at_exclude"),

          show("at_show_methods", "Methods"),
          fluidRow(id = "at_methods",
                   column(width = 6,
                          awesomeRadio("at_zyp",
                                       label = "Prewhitening method",
                                       choices = list("Zhang" = "zhang",
                                                      "Yue-Pilon" = "yuepilon"),
                                       selected = "zhang")),
                   column(width = 6,
                          numericInput("at_alpha", label = "Trend alpha",
                                       value = 0.05, min = 0, max = 0.3, step = 0.05)),
                   bsTooltip("at_zyp", tips$zyp, placement = "left"),
                   bsTooltip("at_alpha", tips$alpha, placement = "left")),
          show("at_show_options", "Data Options"),
          div(id = "at_options",
              fluidRow(
                column(6, selectizeInput("at_annual_percentiles",
                                         label = "Annual perc.",
                                         choices = c(1:99),
                                         selected = c(10,90),
                                         multiple = TRUE)),
                column(6, selectizeInput("at_monthly_percentiles",
                                         label = "Monthly perc.",
                                         choices = c(1:99),
                                         selected = c(10,20),
                                         multiple = TRUE)),
                bsTooltip("at_annual_percentiles", tips$percentiles,
                          placement = "left"),
                bsTooltip("at_monthly_percentiles", tips$percentiles,
                          placement = "left")),

              strong("Low Flows"),
              select_rolling("at_low", set = FALSE, multiple = TRUE),

              selectizeInput("at_percent",
                             label = "Percents of total annual flows",
                             choices = c(1:99),
                             selected = c(25, 33, 50, 75),
                             multiple = TRUE),
              bsTooltip("at_percent", tips$percent, placement = "left"),

              sliderInput("at_normal", label = "Days Outside Normal - Range",
                          value = c(25, 75), min = 1, max = 99, step = 1),
              bsTooltip("at_normal", tips$normal, placement = "left")
          ),
          show("at_show_allowed", "Missing Dates"),
          uiOutput("ui_at_allowed")
      ),

      tabBox(
        width = 9, height = 900,

        ### Plot/Table ---------------------
        tabPanel(
          title = "Exploring Trends",
          withSpinner(DTOutput("at_table_fit")),
          p(style = "margin-bottom:30px"), # A bit of space

          conditionalPanel(
            "output.at_plot",
            helpText("Click on a point or 'lasso' a bunch to add year to ",
                     "'Years to exclude'. Remember to re-Compute ",
                     "Trends.")),
          girafeOutput("at_plot", height = "450px")),

        ### Table ---------------------
        tabPanel(
          title = "Table - Annual Values",
          withSpinner(DTOutput("at_table_years"))
        ),

        ### Info ---------------------
        tabPanel(
          title = "Analysis Info"
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("at_code")
        )
      )
    )
  )

}
