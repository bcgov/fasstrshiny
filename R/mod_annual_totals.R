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

ui_annual_totals <- function(id, plot_height) {

  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Totals"),
      box(width = 3,
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          div(id = ns("seasons_tip"),
              prettySwitch(
                ns("seasons"),
                label = "Include seasons",
                value = formals("plot_annual_cumulative_stats")$include_seasons,
                status = "success", slim = TRUE)),
          bsTooltip(ns("seasons_tip"), tips$seasons, placement = "left"),
          awesomeRadio(ns("discharge"),
                       label = "Discharge type",
                       choices = list("Volumetric Discharge (m3)" = FALSE,
                                      "Runoff Yield (mm)" = TRUE),
                       selected = TRUE),
          bsTooltip(ns("discharge"), tips$discharge, placement = "left")),

      tabBox(
        width = 9,

        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          ggiraph::girafeOutput(ns("plot"), height = plot_height)
        ),

        ### Table ---------------------
        tabPanel(
          title = "Table",
          DT::DTOutput(ns("table"))
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_annual_totals <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    # Plot --------------------
    output$plot <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(!is.null(input$seasons), input$discharge)

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_annual_cumulative_stats", data = "data_flow",
                      input, input_data = data_settings,
                      params_ignore = "discharge",
                      extra = glue::glue("use_yield = {input$discharge}, ",
                                         "include_seasons = {input$seasons}"))

      code$plot <- g

      # Add interactivity
      g <- eval(parse(text = g))


      # Add individual geoms to each plot (annual has more than one)
      for(i in seq_along(g)) {
        g[[i]] <- g[[i]] + ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = paste0("Year: ", Year, "\n",
                                        Statistic, ": ", round(Value, 4)),
                       data_id = Year), size = 3)
      }

      g <- g %>%
        patchwork::wrap_plots(nrow = 2, byrow = FALSE, design = "AC
                                                                 BC")

      ggiraph::girafe(
        ggobj = g, width_svg = 14, height_svg = 6,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(
            css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })


    # Table -----------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      data_flow <- data_raw()

      t <- create_fun("calc_annual_cumulative_stats", data = "data_flow",
                      input, input_data = data_settings,
                      params_ignore = "discharge",
                      extra = glue::glue("use_yield = {input$discharge}, ",
                                         "include_seasons = {input$seasons}"))

      code$table <- t

      parse(text = t) %>%
        eval() %>%
        prep_DT()
    })


    # R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))

  })
}
