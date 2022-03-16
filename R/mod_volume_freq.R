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



# Volume Frequency - High/Low ------------------
ui_volume_freq <- function(id = "vf") {

  fluidRow(
    column(
      width = 12, h2("High/Low Volume Frequency Analysis"),
      box(width = 3,

          # Buttons
          bsButton("compute", "Compute Analysis", style = "primary",
                   class = "centreButton"),
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          hr(class = "narrowHr"),

          # Other
          uiOutput("ui_exclude"),

          show("show_data", "Data"),
          div(id = "data",
              select_rolling(id, set = FALSE, multiple = TRUE),

              fluidRow(
                column(
                  width = 6,
                  awesomeRadio("use_max",
                               label = "Flow type",
                               choices = list("Low" = FALSE,
                                              "High" = TRUE),
                               selected = FALSE, inline = TRUE)),
                column(
                  width = 6, id = "use_log_tip",
                  prettySwitch(
                    "use_log",
                    label = tags$span(strong("Log trans")),
                    value = FALSE, status = "success", slim = TRUE)),
                bsTooltip("use_max", tips$use_max, placement = "left"),
                bsTooltip("use_log_tip", tips$use_log, placement = "left")
              )
          ),
          show("show_plotting", "Plotting"),
          div(id = "plotting",
              fluidRow(
                column(6,
                       awesomeRadio("prob_plot",
                                    label = "Plotting positions",
                                    choices = list("Weibull" = "weibull",
                                                   "Median" = "median",
                                                   "Hazen" = "hazen"))),
                column(6, textInput(
                  "prob_scale",
                  label = "Probabilies to plot",
                  value = "0.9999, 0.999, 0.99, 0.9, .5, .2, .1, .02, .01, .001, .0001"))
              ),
              div(id = "plot_curve_tip",
                  prettySwitch("plot_curve",
                               label = tags$span(strong("Plot curve")),
                               value = TRUE, status = "success", slim = TRUE)),
              bsTooltip("plot_curve_tip", tips$plot_curve, placement = "left"),
              bsTooltip("prob_plot", tips$prob_plot, placement = "left"),
              bsTooltip("prob_scale", tips$prob_scale, placement = "left")
          ),

          show("show_fitting", "Fitting"),
          div(id = "fitting",
              selectizeInput(
                "fit_quantiles",
                label = "Quantiles to estimate",
                choices = seq(0.01, 0.999, 0.0025),
                selected = c(0.975, 0.99, 0.98, 0.95, 0.90,
                             0.80, 0.50, 0.20, 0.10, 0.05, 0.01),
                multiple = TRUE),
              awesomeRadio("fit_distr",
                           label = "Distribution",
                           choices = list("PIII" = "PIII",
                                          "Weibull" = "weibull")),
              awesomeRadio("fit_distr_method",
                           label = "Distribution method",
                           choices = list("Method of Moments (MOM)" = "MOM",
                                          "Maximum Likelihood Estimation (MLE)" = "MLE")),

              bsTooltip("fit_quantiles", tips$fit_quantiles, placement = "left"),
              bsTooltip("fit_distr", tips$fit_distr, placement = "left"),
              bsTooltip("fit_distr_method", tips$fit_distr_method, placement = "left")
          )
      ),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          conditionalPanel(
            "output.plot",
            helpText("Click on a point to add that year to ",
                     "'Years to exclude'. Remember to re-Compute ",
                     "Analysis.")),
          withSpinner(girafeOutput("plot"))
        ),

        ### Table - Plot Data ---------------------
        tabPanel(
          title = "Table - Plot Data",
          withSpinner(DTOutput("table_plot"))
        ),

        ### Table - Fitted Quantiles ---------------------
        tabPanel(
          title = "Table - Fitted Quantiles",
          withSpinner(DTOutput("table_fit"))
        ),

        ### Plot ---------------------
        tabPanel(
          title = "Fit Checks",
          uiOutput("ui_day"),
          verbatimTextOutput("fit_stats"),
          withSpinner(plotOutput("fit_plot", height = "550px"))
        ),

        ### Info ---------------------
        tabPanel(
          title = "Analysis Info"
        ),

        ### R Code ---------------------
        tabPanel(
          title = "R Code",
          verbatimTextOutput("code")
        )
      )
    )
  )

}
