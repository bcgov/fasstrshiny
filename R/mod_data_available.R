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

# Data Availability ---------------------
ui_data_available <- function(id, plot_height) {
  fluidRow(
    column(
      width = 12, h2("Data Availability"),
      tabBox(
        width = 12,

        ### Summary Plot -----------------
        tabPanel(
          title = "Data Summary Plot",
          fluidRow(
            column(
              width = 3,
              helpText("Placeholder descriptive text to describe this section, ",
                       "what it does and how to use it"),
              div(id = NS(id, "availability_tip"),
                  prettySwitch(NS(id, "availability"),
                               label = "Plot availability",
                               value = TRUE,
                               status = "success", slim = TRUE, inline = TRUE)),
              bsTooltip(NS(id, "availability_tip"), tips$availability,
                        placement = "left"),
              selectizeInput(
                NS(id, "stats"),
                label = "Statistics to include",
                choices = eval(formals(plot_data_screening)$include_stat),
                selected = eval(formals(plot_data_screening)$include_stat),
                multiple = TRUE, width = "100%")),
            column(width = 9,
                   ggiraph::girafeOutput(NS(id, "plot1"),
                                         height = plot_height)))),

        ### Availability Plot -----------------
        tabPanel(
          title = "Data Availability Plot",
          fluidRow(
            column(width = 1,
                   awesomeRadio(NS(id, "type"), label = "Plot type",
                                choices = c("Tile" = "tile", "Bar" = "bar")),
                   checkboxGroupButtons(
                     NS(id, "months_inc"),
                     label = "Months",
                     choices = list("Jan" = 1, "Feb" = 2,
                                    "Mar" = 3, "Apr" = 4,
                                    "May" = 5, "Jun" = 6,
                                    "Jul" = 7, "Aug" = 8,
                                    "Sep" = 9, "Oct" = 10,
                                    "Nov" = 11, "Dec" = 12),
                     selected = c(1:12),
                     direction = "vertical"),
                   bsTooltip(NS(id, "months_inc"),
                             "Months to include/exclude from the plot",
                             placement = "left"),
            ),
            column(width = 11, ggiraph::girafeOutput(NS(id, "plot2"),
                                                     height = plot_height))
          )
        ),

        ### Table -----------------
        tabPanel(
          title = "Table",
          DT::dataTableOutput(NS(id, "table"))
        ),

        ### R Code -----------------
        ui_rcode(id)
      )
    )
  )
}


server_data_available <- function(id, data_settings, data_raw, data_loaded) {

  moduleServer(id, function(input, output, session) {

    ## Data --------------
    available_raw <- reactive({
      data_flow <- data_raw()

      d <- create_fun("screen_flow_data", data_name = "data_flow", input,
                      input_data = data_settings)

      code$data <- glue::glue("data_available <- {d}")
      eval(parse(text = d))
    })

    ## Summary plot ------------------
    output$plot1 <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$availability, !is.null(input$stats))

      data_flow <- data_raw()

      e <- c(glue::glue("include_stats = ",
                  "c(\"{glue::glue_collapse(input$stats, sep = '\", \"')}\")"),
             glue::glue("plot_availability = {input$availability}")) %>%
        glue::glue_collapse(sep = ", ")

      g <- create_fun("plot_data_screening", data_name = "data_flow",
                      input, input_data = data_settings, extra = e)

      code$plot <- g

      g <- eval(parse(text = g))[[1]]

      #Add interactivity
      stats <- names(g$data) # Get stats from plot data

      # For tooltips labels...
      names(stats)[stats == "n_missing_Q"] <- "Data Completeness"

      # Add interactive vline
      g <- g + create_vline_interactive(data = g$data, stats = stats, size = 5)


      ggiraph::girafe(
        ggobj = g, width_svg = 13, height_svg = 7,
        options = list(
          ggiraph::opts_toolbar(position = "topleft"),
          ggiraph::opts_selection(type = "none"),
          ggiraph::opts_hover(
            css = "fill:orange; stroke:gray; stroke-opacity:0.5;")))
    })


    ## Missing Data Plot ---------------------------
    output$plot2 <- ggiraph::renderGirafe({
      check_data(data_loaded())
      req(input$type, input$months_inc)

      data_flow <- data_raw()

      g <- create_fun(
        fun = "plot_missing_dates", data_name = "data_flow",
        input, input_data = data_settings,
        params_ignore = c("discharge", "months"),
        extra = glue::glue(
          "months = c({glue::glue_collapse(input$months_inc, sep = ', ')}), ",
          "plot_type = '{input$type}'"))
      code$miss <- g

      g <- eval(parse(text = g))[[1]]

      # Replace layers with interactive
      g$layers[[1]] <- ggiraph::geom_tile_interactive(
        colour = "grey",
        ggplot2::aes(fill = Percent_Missing,
                     tooltip = glue::glue(
                       "Year: {Year}\n",
                       "Month: {Month}\n",
                       "Missing Days: {Missing} ({Percent_Missing}%)",
                       .trim = FALSE),
                     data_id = glue::glue("{Year}-{Month}")))

      g <- g +
        ggplot2::guides(fill = ggplot2::guide_coloursteps(
          even.steps = FALSE,
          show.limits = TRUE))

      ggiraph::girafe(ggobj = g, width_svg = 14, height_svg = 7,
                      options = list(ggiraph::opts_selection(type = "none")))
    })

    ## Summary table ------------------
    output$table <- DT::renderDT({
      check_data(data_loaded())

      available_raw() %>%
        dplyr::select(-dplyr::contains("STATION_NUMBER")) %>%
        dplyr::rename("Total days" = "n_days",
                      "Total flow days" = "n_Q",
                      "Total missing days" = "n_missing_Q",
                      "Standard deviation" = "StandardDeviation") %>%
        dplyr::rename_with(
          .cols = dplyr::ends_with("_missing_Q"),
          ~ stringr::str_replace(., "_missing_Q", " missing days")) %>%
        prep_DT()
    })

    ## R Code -----------------
    code <- reactiveValues()
    output$code <- renderText(code_format(code))
  })
}
