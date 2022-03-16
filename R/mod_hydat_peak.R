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

# HYDAT Peaks ------------------
ui_hydat_peak <- function(id = "hp") {

  fluidRow(
    column(
      width = 12, h2("HYDAT Peak Volume Frequency Analysis"),
      box(width = 3,

          bsButton("hp_compute", "Compute Analysis", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, what it does and how to use it"),
          hr(class = "narrowHr"),

          fluidRow(
            column(width = 6,
                   awesomeRadio("hp_use_max",
                                label = "Flow type",
                                choices = list("Low" = FALSE,
                                               "High" = TRUE),
                                selected = FALSE)),
            column(width = 6,
                   awesomeRadio("hp_fit_distr",
                                label = "Distribution",
                                choices = list("PIII" = "PIII",
                                               "Weibull" = "weibull")))
          ),
          bsTooltip("hp_use_max", tips$use_max, placement = "left"),
          bsTooltip("hp_fit_distr", tips$fit_distr, placement = "left"),

          selectizeInput(
            "hp_fit_quantiles",
            label = "Quantiles to estimate",
            choices = seq(0.01, 0.999, 0.0025),
            selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                         0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
            multiple = TRUE),
          bsTooltip("hp_fit_quantiles", tips$fit_quantiles, placement = "left"),

          fluidRow(
            column(width = 6, id = "hp_plot_curve_tip",
                   prettySwitch("hp_plot_curve",
                                label = "Plot curve", value = TRUE,
                                status = "success", slim = TRUE)),
            column(width = 6, id = "hp_use_log_tip",
                   prettySwitch("hp_use_log",
                                label = "Log trans", slim = TRUE,
                                value = FALSE, status = "success"))
          ),
          bsTooltip("hp_plot_curve_tip", tips$plot_curve, placement = "left"),
          bsTooltip("hp_use_log_tip", tips$use_log, placement = "left"),

          fluidRow(
            column(6, awesomeRadio("hp_prob_plot",
                                   label = "Plotting positions",
                                   choices = list("Weibull" = "weibull",
                                                  "Median" = "median",
                                                  "Hazen" = "hazen"))),
            column(6, textInput(
              "hp_prob_scale",
              label = "Probabilies to plot",
              value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001"))
          ),
          bsTooltip("hp_prob_plot", tips$prob_plot, placement = "left"),
          bsTooltip("hp_prob_scale", tips$prob_scale, placement = "left"),
      ),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          withSpinner(girafeOutput("hp_plot"))
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          withSpinner(DTOutput("hp_table"))
        ),

        ### Plot ---------------------
        tabPanel(
          title = "Fit Checks",
          verbatimTextOutput("hp_fit_stats"),
          withSpinner(plotOutput("hp_fit_plot", height = "550px"))
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("hp_code")
        )
      )
    )
  )
}
