
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
  geom_vline_interactive(aes(xintercept = .data[[date]],
                             tooltip = eval(parse(text = tips)),
                             data_id = .data[[date]]),
                         alpha = alpha, size = size)
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
