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


          #   tags$blockquote("fasstrshiny is an R Shiny app offering a user interface to the various functions from the",
          #                   code(a(href = "https://bcgov.github.io/fasstr", "fasstr")), "R package."),

          # tags$ul(
          #   tags$li("See the '", strong("Overview"), "' tab for how to use this app"),
          #   tags$li("See the '", strong("Working in R"), "' tab for how to set up",
          #           code("fasstr"), " and ", code("fasstrshiny"), "on your own computer, ",
          #           "and for how to use the code output from ", code("fasstrshiny"))
          # ),

          #  hr(),
          p(),

          #   h4("About"),

          tags$blockquote(
            strong("fasstrshiny"),
            p(),
            "Flow Analysis Summary Statistics for R - Shiny",
            p(),
            glue::glue("v{packageVersion('fasstrshiny')}"), " - June 2022",
            p()
          ),
          "This app is developed primarily to provide various tools to summarize, analyze, trend, and ",
          "visualize streamflow data. It offers a user interface to the various functions from the ",
          code(a(href = "https://bcgov.github.io/fasstr", "fasstr")), "R package.",
          "This app summarizes continuous daily mean streamflow data into various daily, monthly, annual, and ",
          "long-term statistics (e.g. mean, maximum, minimum, median, and other percentiles), completes annual ",
          "trending and frequency analyses, with outputs in both table and ",
          "plot formats and provided R code to replicate outputs in R scripts.",

          p(),
          tags$ul(
            tags$li("See the '", strong("Overview"), "' tab for how to use this app."),
            tags$li("See the '", strong("Working in R"), "' tab for how to set up",
                    code("fasstr"), " and ", code("fasstrshiny"), "on your own computer, ",
                    "and for how to use the code output from ", code("fasstrshiny"),".")
          ),

          hr(),
          p(),

          h4("Use of HYDAT Data"),

          "This app and the fasstr package rely on the ", a(href = "https://bcgov.github.io/tidyhydat", "tidyhydat"),
          "package which provides easy access to hydrometric data and station information from the ",
          a(href = "https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/
            survey/data-products-services/national-archive-hydat.html",
            "Canadian National Water Data Archive (HYDAT)."),
          "Daily mean flow data, annual instantaneous peak data, and station information (as seen in the ",
          "'Data' >> 'Loading' tab) are provided directly from the HYDAT database.",
          " This hydrometric information is under the ",
          a(href = "https://open.canada.ca/en/open-government-licence-canada",
            "Open Government Licence"),
          " and has its own ",
          a(href = "https://wateroffice.ec.gc.ca/disclaimer_info_e.html",
            "data disclaimer."),
          p(),
          "Reported statistics in plots and tables within this app, beyond the provided HYDAT or other data sets, ",
          "are the result of the summarization and calculation of statistics using the fasstr R functions and not ",
          "from the HYDAT database.",

          p(),
          hr(),
          # Disclaimer -----------------------
          h4("Disclaimers", style = "margin-left:0"),
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
            "specifically advised of the possibility of such damages."),

          hr(),
          h4("Versions", style = "margin-left:0"),
          p(
            strong(
              HTML(
                paste0(
                  "Using ",
                  a(href = 'https://github.com/bcgov/fasstrshiny', code('fasstrshiny'), target="_blank"),
                  glue::glue("v{packageVersion('fasstrshiny')}, "),
                  a(href = "http://github.com/bcgov/fasstr", code('fasstr'), target="_blank"),
                  glue::glue("v{packageVersion('fasstr')}, and "),
                  a(href = "https://github.com/ropensci/tidyhydat", code('tidyhydat'), target="_blank"),
                  glue::glue("v{packageVersion('tidyhydat')} to access "),
                  a(href = file.path("https://www.canada.ca/en",
                                     "environment-climate-change/services",
                                     "water-overview/quantity/monitoring",
                                     "survey/data-products-services",
                                     "national-archive-hydat.html"),
                    target="_blank",
                    "HYDAT data")),
                glue::glue_data(tidyhydat::hy_version(),
                                "v{Version} ({substr(Date, 1, 10)})"))))

        ),

        # Overview --------------------------------
        tabPanel(
          title = "Overview", width = 12,
          includeMarkdown(system.file("md", "overview.md", package = "fasstrshiny")),
        ),

        # R Workflow - Setup -------------------------------
        tabPanel(
          title = "Working in R", width = 12,
          includeMarkdown(system.file("md", "setup.md", package = "fasstrshiny"))
        ),

        # Glossary? -------------------------------
        tabPanel(
          title = "Glossary", width = 12,
          "insert something here?"
        )
      )
    )
  )
}
