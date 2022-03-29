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

# Annual Means -----------------------
ui_annual_means <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 12, h2("Annual Means"),
      tabBox(
        width = 12,
        ### Plot ---------------------
        tabPanel(
          title = "Plot",
          helpText("Placeholder descriptive text to describe this section, ",
                   "what it does and how to use it"),
          ui_download(id = ns("plot")),
          select_plot_options(select_plot_title(id)),
          ggiraph::girafeOutput(ns("plot"), height = opts$plot_height)
        ),


        # R Code ---------------------
        ui_rcode(id)
      )
    )
  )
}

server_annual_means <- function(id, data_settings, data_raw,
                                data_loaded, data_code) {

  moduleServer(id, function(input, output, session) {

    # Plot --------------------
    plot <- reactive({
      check_data(data_loaded())

      data_flow <- data_raw()

      g <- create_fun(fun = "plot_annual_means", data_name = "data_flow",
                      input, input_data = data_settings(),
                      params_ignore = "discharge")

      code$plot <- g
      labels$plot <- "Plotting annual means"

      g <- eval_check(g)[[1]]

      # Add title
      if(input$plot_title) {
        g <- g +
          ggplot2::ggtitle(plot_title(data_settings(), "Annual Means")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
      }



      # Replace layers with interactive
      g$layers[[1]] <- ggiraph::geom_bar_interactive(
        ggplot2::aes(tooltip = glue::glue("Year: {.data$Year}\n",
                                          "MAD Diff: {round(.data$MAD_diff, 4)}",
                                          .trim = FALSE),
                     data_id = .data$Year, colour = "Annual MAD difference"),
        fill = "cornflowerblue", stat = "identity")

      g <- g + ggplot2::geom_hline(ggplot2::aes(yintercept = 0,
                                                colour = "Long-term MAD"),
                                   size = 1) +
        ggplot2::scale_colour_manual(
          values = c("Long-term MAD" = "black",
                     "Annual MAD difference" = "cornflowerblue")) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(
          fill = c("black", "cornflowerblue")))) +
        ggplot2::theme(legend.position = c(0.8, 0.8),
                       legend.title = ggplot2::element_blank(),
                       legend.key = ggplot2::element_rect(colour = NA))

      g
    })

    dims <- c(14, 4) * opts$scale

    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = plot(), width_svg = dims[1], height_svg = dims[2],
                      options = ggiraph_opts())
    })

    # Download Plot -----------------
    download(id = "plot", plot = plot, name = "mad", data_settings, dims)


    # R Code -----------------
    code <- reactiveValues()
    labels <- reactiveValues()
    output$code <- renderText(code_format(code, labels, data_code))

  })
}
