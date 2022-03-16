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

# Home -------------------------
ui_home <- function(id) {
  fluidRow(
    column(
      width = 12, h2("Welcome to fasstrshiny"),

      tabBox(
        width = 12,

        # Disclaimer
        tabPanel(
          title = "Disclaimer", width = 12,
          p("This is an R Shiny app offering a user interface to the fasstr R package.",
            "To get started, first go to the Data tab on the Navigation menu, ",
            "choose a HYDAT station and load some data!"),
          p(strong("(Note that loading CSV data is not yet implemented)")),
          br(),
          p("Once you have loaded data you'll be able to explore the other tabs."),
          p("Remeber that this is a work in progress so keep track of what you like,",
            "don't like and what broke so we can make it better!")
        ),

        # Overview
        tabPanel(
          title = "Overview", width = 12,
          p("Blah, blah, blah")
        ),

        # R Workflow - Setup
        tabPanel(
          title = "Getting Setup", width = 12,
          p("Blah, blah, blah")
        )
      )
    )
  )
}
