code_format <- function(code) {
  names(code) %>%
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
    glue::glue_collapse("\n") %>%
    stringr::str_remove_all("^\n*")
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
  ggiraph::geom_vline_interactive(
    ggplot2::aes(xintercept = .data[[date]],
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

prep_hydat <- function() {

  # tidyhydat Stations data -----------------------
  stations_list <- tidyhydat::hy_stn_data_range(prov_terr_state_loc = "BC") %>%
    dplyr::filter(DATA_TYPE == "Q") %>%
    dplyr::pull(STATION_NUMBER)


  ## Create a dataframe of all station metadata and a list of all stations
  stations <- tidyhydat::hy_stations(station_number = stations_list) %>%
    dplyr::left_join(tidyhydat::hy_agency_list(), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>%
    dplyr::rename("CONTRIBUTOR" = AGENCY_EN) %>%
    dplyr::left_join(tidyhydat::hy_agency_list(), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%
    dplyr::rename("OPERATOR" = AGENCY_EN) %>%
    dplyr::left_join(tidyhydat::hy_datum_list(), by = c("DATUM_ID" = "DATUM_ID")) %>%
    dplyr::rename("DATUM" = DATUM_EN) %>%
    dplyr::mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>%
    dplyr::left_join(tidyhydat::hy_reg_office_list(),
                     by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>%
    dplyr:: rename("REGIONAL_OFFICE" = REGIONAL_OFFICE_NAME_EN) %>%
    dplyr::left_join(tidyhydat::hy_stn_regulation(), by="STATION_NUMBER") %>%
    dplyr::select(STATION_NUMBER, STATION_NAME, PROV_TERR_STATE_LOC, HYD_STATUS,
                  LATITUDE, LONGITUDE, DRAINAGE_AREA_GROSS, RHBN,
                  REAL_TIME, REGULATED,CONTRIBUTOR, OPERATOR, REGIONAL_OFFICE, DATUM) %>%
    dplyr::mutate(RHBN = ifelse(RHBN, "YES", "NO"),
                  REAL_TIME = ifelse(REAL_TIME, "YES", "NO"),
                  REGULATED = ifelse(REGULATED, "YES", "NO"),
                  DRAINAGE_AREA_GROSS = round(DRAINAGE_AREA_GROSS, digits = 2))

  station_parameters <- tidyhydat::hy_stn_data_range() %>%
    dplyr::filter(DATA_TYPE == "Q"| DATA_TYPE == "H")  %>%
    dplyr::select(STATION_NUMBER, DATA_TYPE) %>% tidyr::spread(DATA_TYPE, DATA_TYPE) %>%
    dplyr::mutate(PARAMETERS = dplyr::case_when(is.na(H) ~ "FLOW",
                                                is.na(Q) ~ "LEVEL",
                                                TRUE ~ paste("FLOW AND LEVEL")))

  dplyr::left_join(stations,
                   dplyr::select(station_parameters, STATION_NUMBER, PARAMETERS),
                   by = "STATION_NUMBER") %>%
    dplyr::rename_all(stringr::str_to_lower) %>%
    dplyr::rename("province" = "prov_terr_state_loc") %>%
    dplyr::mutate(dplyr::across(c(-"station_number", -"province", -"latitude", -"longitude",
                                  "drainage_area_gross"), stringr::str_to_sentence))
}

