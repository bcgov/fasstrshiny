
## Other Functions ---------

code_format <- function(code, id) {
  stringr::str_subset(names(code), glue::glue("{id}_")) %>%
    sort() %>%
    purrr::map(~code[[.]]) %>%
    as.character() %>%
    stringr::str_remove_all("^\\{|\\}$") %>%
    stringr::str_squish() %>%
    glue::glue_collapse("\n\n") %>%
    stringr::str_replace_all("%>%", "%>%\n ") %>%
    stringr::str_replace_all("\\+", "\\+\n ") %>%
    stringr::str_replace_all("&&", "\n\n") %>%
    code_break_lines()
}

code_break_lines <- function(code) {
  s <- stringr::str_split(code, "\n") %>% unlist()
  l <- purrr::map_lgl(s, ~nchar(.) > 80)
  # only set new line if list items are longer than 4 and not near the end of a line
  s[l] <- stringr::str_replace_all(s[l], ",(?! [:print:]{1,4}(,|[:punct:]{1,4}$))", ",\n")

  glue::glue_collapse(s, "\n") %>%
    styler::style_text() %>%
    as.character() %>%
    glue_collapse("\n") %>%
    str_remove_all("^\n*")
}



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


to_girafe <- function(g, value = NULL, type = "trends", digits = 4) {

  if(type == "trends") {
    g$layers[[1]] <- ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = paste0(
        "Year: ", Year, "\n",
        value, ": ", round(Value, digits = digits)),
        data_id = Year), size = 4)
  } else if(type == "flow") {
    g$layers[[1]] <- ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = paste0(
        "Year: ", Year, "\n",
        "Discharge: ", round(Value, digits = digits), "\n",
        "Probability: ", round(prob, digits = digits)),
        data_id = Year), size = 2)
    g <- g + ggplot2::scale_color_viridis_d(end = 0.8)
  } else if(type == "raw") {
    g$layers[[1]] <- ggiraph::geom_line_interactive(
      ggplot2::aes(tooltip = paste0(
        "Date: ", Date, "\n",
        value, ": ", round(Value, digits = digits)),
        data_id = Date), colour = "dodgerblue4",
      na.rm = TRUE)
  }

  ggiraph::girafe(ggobj = g, width_svg = 8, height_svg = 5,
                  options = list(ggiraph::opts_selection(type = "multiple")))
}



text_to_num <- function(x) {
  suppressWarnings(as.numeric(stringr::str_split(x, ",", simplify = TRUE)))
}


get_date <- function(n, water_year) {
  d <- as.Date(as.numeric(n), origin = as.Date("1900-01-01") - 1)
  if(water_year != 1) {
    d[as.numeric(format(d, "%m")) >= water_year] <-
      d[as.numeric(format(d, "%m")) >= water_year] - 365
  }
  d
}

find_hydat <- function() {
  h <- "Hydat.sqlite3"
  locs <- c("local" = file.path(tidyhydat::hy_dir(), h),
            "dev" = file.path("inst", "shiny_app", h),
            "shinyapps" = h)

  locs <- locs[file.exists(locs)]

  if(length(h) == 0) {
    stop("Cannot find 'Hydat.sqlite3' needed by tidyhydat. Consider running ",
         "tidyhydat::download_hydat()", call. = FALSE)
  }
  h[1]
}
