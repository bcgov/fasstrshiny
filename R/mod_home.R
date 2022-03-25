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
      width = 12, h2("Welcome to ", code("fasstrshiny")),

      tabBox(
        width = 12,

        # Disclaimer
        tabPanel(
          title = "Welcome", width = 12,


          "This is an R Shiny app offering a user interface to the ",
          code(a(href = "https://bcgov.github.io/fasstr", "fasstr")), "R package.",
          p(),

          tags$ul(
            tags$li("See the '", strong("Overview"), "' tab for how to use this app"),
            tags$li("If you would like to install ", code("fasstr"), " and ",
               code("fasstrshiny"), "on your own computer, see the '",
               strong("Getting Setup"), "' tab")),


          hr(),
          # Disclaimer -----------------------
          h5("Warranty Disclaimer"),
          p("This information is provided as a public service by the Government ",
            "of British Columbia, Box 9411, Victoria, British Columbia, Canada ",
            "V8W 9V1."),

          p("This website and all of the information it contains are provided ",
            "\"as is\" without warranty of any kind, whether express or implied. ",
            "All implied warranties, including, without limitation, implied ",
            "warranties of merchantability, fitness for a particular purpose, ",
            "and non-infringement, are hereby expressly disclaimed. Links and ",
            "references to any other websites are provided for information only ",
            "and listing shall not be taken as endorsement of any kind. The ",
            "Government of British Columbia is not responsible for the content ",
            "or reliability of the linked websites and does not endorse the ",
            "content, products, services or views expressed within them."),

          h5("Limitation of Liabilities"),
          p("Under no circumstances will the Government of British Columbia ",
            "be liable to any person or business entity for any direct, ",
            "indirect, special, incidental, consequential, or other damages ",
            "based on any use of this website or any other website to which ",
            "this site is linked, including, without limitation, any lost ",
            "profits, business interruption, or loss of programs or ",
            "information, even if the Government of British Columbia has been",
            "specifically advised of the possibility of such damages.")
        ),

        # Overview --------------------------------
        tabPanel(
          title = "Overview", width = 12,

          "To get started, first go to the Data tab on the Navigation menu, ",
          "choose a HYDAT station and load some data!",

          br(),
          p("Once you have loaded data you'll be able to explore the other tabs."),
          p("Remeber that this is a work in progress so keep track of what you like,",
            "don't like and what broke so we can make it better!"),
        ),

        # R Workflow - Setup -------------------------------
        tabPanel(
          title = "Getting Setup", width = 12,
          p("Blah, blah, blah")
        )
      )
    )
  )
}
