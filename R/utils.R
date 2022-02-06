
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

create_vline_interactive <- function(data, stats, date_fmt = "%b %d", digits = 4) {
  x <- stats[1]

  date <- glue::glue("'Date: ', format(.data[['{x}']], '{date_fmt}')")
  s <- stats[2:length(stats)]
  s <- glue::glue("'{names(s)}: ', round(.data[['{s}']], digits = {digits})")
  s <- glue::glue_collapse(c(date, s), sep = ", '\n', ")

  s <- paste0("paste0(", s, ")")

  geom_vline_interactive(aes(xintercept = .data[[x]],
                             tooltip = eval(parse(text = s)),
                             data_id = .data[[x]]), alpha = 0.01)
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

  locs <- locs[file.exists(locs)] %>%
    normalizePath()

  if(length(h) == 0) {
    stop("Cannot find 'Hydat.sqlite3' needed by tidyhydat. Consider running ",
         "tidyhydat::download_hydat()", call. = FALSE)
  }
  locs[1]
}
