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

# Formatting Data Table Output -----------------------------------------

prep_DT <- function(data, digits = 4) {
  data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., .env$digits))) %>%
    DT::datatable(rownames = FALSE,
                  filter = 'top',
                  extensions = c("Scroller", "Buttons"),
                  selection = "single",
                  options = list(scrollX = TRUE, scrollY = 450, scroller = TRUE,
                                 deferRender = TRUE, dom = 'Brtip',
                                 buttons = c('copy', 'csv', 'excel')))
}

#' Set ggiraph options
#'
#' @noRd
ggiraph_opts <- function(selection = "none") {
  list(
    ggiraph::opts_toolbar(position = "topright"),
    ggiraph::opts_selection(type = selection,
                            css = "fill:red; stroke:gray; r:5pt"),
    ggiraph::opts_hover(
      css = "fill:orange; stroke:gray; stroke-opacity:0.5; fill-opacity:1;"))
}




# Styling Code -----------------------------------------------------------
code_order <- function(nm, order) {
  nm <- sort(nm)
  c(order[order %in% nm], nm[!nm %in% order])
}

code_format <- function(code, labels, data_code = NULL,
                        order = c("data_raw", "data", "plot", "table")) {

  if(is.reactivevalues(code)) code <- reactiveValuesToList(code)
  if(is.reactivevalues(labels)) labels <- reactiveValuesToList(labels)
  if(is.reactive(data_code)) data_code <- data_code()

  if(!is.null(data_code)) {
    code$data_raw <- data_code
    labels$data_raw <- "Load flow data"
  }
  order <- code_order(names(code), order)

  code_formatted <- purrr::map(code, ~ {
    as.character(.x) %>%
      stringr::str_remove_all("^\\{|\\}$") %>%
      stringr::str_squish() %>%
      glue::glue_collapse("\n\n") %>%
      stringr::str_replace_all("%>%", "%>%\n ") %>%
      stringr::str_replace_all("\\+", "\\+\n ") %>%
      stringr::str_replace_all("&&", "\n\n") %>%
      code_break_lines()
  })

  purrr::map2(labels[order], code_formatted[order], ~glue::glue("# {.x}\n{.y}")) %>%
    glue::glue_collapse(sep = "\n\n")

}

code_break_lines <- function(code) {
  s <- stringr::str_split(code, "\n") %>% unlist()

  # Which lines too long?
  l <- purrr::map_lgl(s, ~nchar(.) > 100)

  # Split after ),
  # only set new line if list items are longer than 4 and not near the end of a line
  s[l] <- stringr::str_replace_all(s[l], "\\),(?! [:print:]{1,4}(,|[:punct:]{1,4}$))", "),\n")
  s <- stringr::str_split(s, "\n") %>% unlist()
  l <- purrr::map_lgl(s, ~nchar(.) > 100)

  # Still too long? split after ,
  while(any(l)){
    s[l] <- stringr::str_replace(s[l], ",(?! [:print:]{1,4}(,|[:punct:]{1,4}$))", ",\n")
    s <- stringr::str_split(s, "\n") %>% unlist()
    l <- purrr::map_lgl(s, ~nchar(.) > 100)
  }

  glue::glue_collapse(s, "\n") %>%
    styler::style_text() %>%
    as.character() %>%
    glue::glue_collapse("\n") %>%
    stringr::str_remove_all("^\n*")
}


# Plotting functions -----------------------------------------

#' Create a ggplot patchwork of the fitdistrplus fitting plots
#' @noRd
gg_fitdistr <- function(fit, title) {

  g <- patchwork::wrap_plots(
    fitdistrplus::denscomp(fit, addlegend = FALSE, plotstyle = "ggplot"),
    fitdistrplus::qqcomp(fit, addlegend = FALSE, plotstyle = "ggplot"),
    fitdistrplus::cdfcomp(fit, addlegend = FALSE, plotstyle = "ggplot"),
    fitdistrplus::ppcomp(fit, addlegend = FALSE, plotstyle = "ggplot")) +
    patchwork::plot_annotation(title = title)

  g &
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}

#' Create a vline interactive tooltip to add to ggiraph plots
#' @noRd
create_vline_interactive <- function(data, stats, date_fmt = "%b %d",
                                     combine = FALSE, digits = 4, size = 1,
                                     alpha = 0.005) {

  # If not named (NULL or ""/NA), make name from value
  if(is.null(names(stats))) {
    names(stats) <- stats
  } else {
    names(stats)[is.na(names(stats)) | names(stats) == ""] <-
      stats[is.na(names(stats)) | names(stats) == ""]
  }

  # Split stats into first stat (usually Year, Month or Date) and the rest
  date <- stats[1]
  stats <- stats[2:length(stats)]

  # Check if first is date or date/time, if not date_fmt is NULL
  if(!any(class(data[[date]]) %in% c("Date", "POSIXct"))) date_fmt <- NULL

  # If date/datetime, format appropriately otherwise leave as is
  if(is.null(date_fmt)) {
    date_tt <- glue::glue("'{names(date)}: ', .data[['{date}']]")
  } else {
    date_tt <- glue::glue("'Date: ', format(.data[['{date}']], '{date_fmt}')")
  }

  # All stats except the first are assumed to be numeric
  stats_fct <- stats[!sapply(data[, stats], is.numeric)]
  stats_num <- stats[!stats %in% stats_fct]

  stats_tt <- c(
    glue::glue("'{names(stats_fct)}: ', .data[['{stats_fct}']]"),
    glue::glue("'{names(stats_num)}: ', round(.data[['{stats_num}']], digits = {digits})"))

  # Combine
  tips <- glue::glue_collapse(c(date_tt, stats_tt), sep = ", '\n', ")
  tips <- paste0("paste0(", tips, ")")

  # First stats is assumed to be X value and data_id
  ggiraph::geom_vline_interactive(
    ggplot2::aes(xintercept = .data[[date]],
                 tooltip = eval(parse(text = tips)),
                 data_id = .data[[date]]),
    alpha = alpha, size = size)
}



# Minor utility functions -----------------------------------

#' Get CSS files
#' @noRd
get_css <- function() {
  css <- system.file("shiny_app", "www", "bcgov.css", package = "fasstrshiny")
  if(css == "" ) {
    if(file.exists("../../shiny_app/www/bcgov.css")) {
      css <- "../../shiny_app/www/bcgov.css"
    } else css <- ""
  }
  css
}

#' Get equation
#' @noRd
equation <- function(fit) {

  if(fit$distname == "PIII") {

    a <- fit$estimate[["shape"]] %>% round(2)
    s <- fit$estimate[["scale"]] %>% round(2)
    l <- fit$estimate[["location"]] %>% round(2)

    e <- glue::glue(
      "f(x) = \\frac{1}{<<s>>^<<a>> \\Gamma(<<a>>)}",
      "(x-<<l>>)^{<<a>>-1} e^{-(\\frac{x-<<l>>}{<<s>>})}",
      .open = "<<", .close = ">>") %>%
      katex::katex_html(output = "mathml", preview = FALSE)
  } else if(fit$distname == "weibull") {

    a <- fit$estimate[["shape"]] %>% round(2)
    b <- fit$estimate[["scale"]] %>% round(2)

    e <- glue::glue(
      "f(x) = \\frac{<<a>>}{<<b>>}",
      "\\frac{x}{<<b>>}^{<<a>>-1} e^{-(\\frac{x}{<<b>>})^{<<a>>}}",
      .open = "<<", .close = ">>") %>%
      katex::katex_html(output = "mathml", preview = FALSE)
  }
  e
}


#' Convert character strings to numeric vectors
#' @noRd
text_to_num <- function(x) {
  suppressWarnings(as.numeric(stringr::str_split(x, ",", simplify = TRUE)))
}


#' Return the *plotting date* for a day of year given the water water year
#' @noRd
get_date <- function(n, water_year) {
  d <- as.Date(as.numeric(n), origin = as.Date("1900-01-01") - 1)
  if(water_year != 1) {
    d[as.numeric(format(d, "%m")) >= water_year] <-
      d[as.numeric(format(d, "%m")) >= water_year] - 365
  }
  d
}

#' Turn Day of Year into a date given a year
#' @noRd
yday_as_date <- function(yday, year) {
 as.Date(yday, origin = paste0(year, "-01-01")) - 1
}

#' Get an SVG of the ggiraph lasso for use in messages
#' @noRd
lasso_svg <- function() {
  shiny::HTML("
<svg xmlns='http://www.w3.org/2000/svg' width='10pt' height='10pt' viewBox='0 0 230 230' stroke = '#069'><g><ellipse ry='65.5' rx='86.5' cy='94' cx='115.5' stroke-width='20' fill='transparent'></ellipse><ellipse ry='11.500001' rx='10.5' cy='153' cx='91.5' stroke-width='20' fill='transparent'></ellipse><line y2='210.5' x2='105' y1='164.5' x1='96' stroke-width='20'></line></g></svg>")
}

#' Create pretty consecutive text lists of numbers
#' e.g., c(1, 2, 3, 4, 6, 7, 8, 9) ===> c(1:4, 6:9)
#' @noRd
conseq <- function(s, type = "num", wrap = NULL) {

  if(is.null(wrap) && type == "num") wrap <- TRUE
  if(is.null(wrap) && type == "month") wrap <- FALSE

  s <- sort(as.numeric(s))

  if(length(s) == 1) {
    if(type == "num") return(as.character(s))
    if(type == "month") return(month.abb[s])
  }

  dif <- s[seq(length(s))][-1] - s[seq(length(s)-1)]
  new <- !c(0, dif == 1)
  cs <- cumsum(new)
  res <- vector(mode="list", max(cs))
  for(i in seq(res)){
    s.i <- s[which(cs == i)]
    if(length(s.i) > 2){
      if(type == "num") res[[i]] <- paste(min(s.i), max(s.i), sep=":")
      if(type == "month") res[[i]] <- paste(month.abb[min(s.i)],
                                            month.abb[max(s.i)], sep="-")
    } else {
      if(type == "num") res[[i]] <- as.character(s.i)
      if(type == "month") res[[i]] <- as.character(month.abb[s.i])
    }
  }

  res <- paste(unlist(res), collapse = ", ")

  if(wrap) res <- paste0("c(", res, ")")

  res
}

plot_title <- function(settings, desc = "") {

  wy <- ""
  months_chr <- ""

  if(desc != "") desc <- glue::glue("{desc}: ")
  if(settings$water_year != 1) wy <- "Water Year "
  if(!(all(settings$months %in% 1:12) & all(1:12 %in% settings$months))) {
    months_chr <- paste0(conseq(settings$months, type = "month"), " ")
  }

  glue::glue_data(
    settings,
    "{desc}{station_name} ",
    "({months_chr}{wy}{years_range[1]}-{years_range[2]})")
}


#' Fetch default values of an argument in a function
#' @noRd
default <- function(fun, arg) eval(formals(fun)[[arg]])



# HYDAT functions ----------------------------
find_hydat <- function() {
  h <- "Hydat.sqlite3"
  locs <- c("local" = file.path(tidyhydat::hy_dir(), h),
            "dev" = file.path("inst", "shiny_app", h),
            "shinyapps" = h)

  locs <- locs[file.exists(locs)] %>%
    normalizePath()

  if(length(h) == 0) {
    stop("Cannot find 'Hydat.sqlite3' needed by tidyhydat. Consider running ",
         "tidyhydat::download_hydat()", call. = FALSE)
  }
  locs[1]
}

prep_hydat <- function(bc_only = TRUE) {
  if(bc_only) prov <- "BC" else prov <- NULL
  stations_list <- tidyhydat::hy_stn_data_range(prov_terr_state_loc = prov) %>%
    dplyr::filter(.data$DATA_TYPE == "Q") %>%
    dplyr::pull(.data$STATION_NUMBER)

  ## Create a dataframe of all station metadata and a list of all stations
  stations_raw <- tidyhydat::hy_stations(station_number = stations_list) %>%
    dplyr::left_join(tidyhydat::hy_stn_regulation(), by = "STATION_NUMBER") %>%
    dplyr::select("STATION_NUMBER", "STATION_NAME", "PROV_TERR_STATE_LOC",
                  "HYD_STATUS", "LATITUDE", "LONGITUDE", "DRAINAGE_AREA_GROSS",
                  "RHBN", "REAL_TIME", "REGULATED") %>%
    dplyr::mutate(RHBN = dplyr::if_else(.data$RHBN, "Yes", "No"),
                  REAL_TIME = dplyr::if_else(.data$REAL_TIME, "Yes", "No"),
                  REGULATED = dplyr::if_else(.data$REGULATED, "Yes", "No"),
                  DRAINAGE_AREA_GROSS = round(.data$DRAINAGE_AREA_GROSS, digits = 2),
                  STATION_NAME = stringr::str_to_title(.data$STATION_NAME),
                  HYD_STATUS = stringr::str_to_title(.data$HYD_STATUS))

  station_parameters <- tidyhydat::hy_stn_data_range(stations_list) %>%
    dplyr::filter(.data$DATA_TYPE == "Q"| .data$DATA_TYPE == "H")  %>%
    dplyr::select("STATION_NUMBER", "DATA_TYPE") %>%
    tidyr::pivot_wider(names_from = .data$DATA_TYPE,
                       values_from = .data$DATA_TYPE) %>%
    dplyr::mutate(PARAMETERS = dplyr::case_when(is.na(.data$H) ~ "Flow",
                                                is.na(.data$Q) ~ "Level",
                                                TRUE ~ paste("Flow and Level"))) %>%
    dplyr::select("STATION_NUMBER", "PARAMETERS") %>%
    dplyr::left_join(
      tidyhydat::hy_stn_data_range() %>%
        dplyr::filter(.data$DATA_TYPE == "Q")  %>%
        dplyr::select("STATION_NUMBER", "Year_from", "Year_to", "RECORD_LENGTH"),
      by = "STATION_NUMBER")

  dplyr::left_join(stations_raw,
                   station_parameters,
                   by = "STATION_NUMBER") %>%
    dplyr::rename("PROVINCE" = "PROV_TERR_STATE_LOC") %>%
    dplyr::mutate(WSC_SUBSUB_DRAINAGE = substr(.data$STATION_NUMBER, 1, 4)) %>%
    dplyr::select("STATION_NUMBER", "STATION_NAME", "PROVINCE", "PARAMETERS",
                  "HYD_STATUS", "REGULATED", "REAL_TIME", "RHBN",
                  "DRAINAGE_AREA_GROSS", "Year_from", "Year_to",
                  "RECORD_LENGTH", "WSC_SUBSUB_DRAINAGE",
                  "LATITUDE", "LONGITUDE")
}
