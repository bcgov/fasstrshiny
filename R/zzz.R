.onLoad <-
  function(libname = find.package("fasstrshiny"),
           pkgname = "fasstrshiny") {
    # CRAN Note avoidance
    if (getRversion() >= "2.15.1")
      utils::globalVariables(
        # Vars used in Non-Standard Evaluations, declare here to avoid CRAN warnings
        ## This is getting ridiculous
        c("LATITUDE",
          "LONGITUDE",
          "LTMAD",
          "Max_Value",
          "Mean",
          "Min_Value",
          "Month",
          "Number of Years",
          "Ptile1",
          "Ptile2",
          "STATION_NUMBER",
          "StationNum",
          "WaterYear",
          "n_na",
          "tooltip",
          "watershed_exists",
          ".",
          "Value",
          "stn")
      )
    invisible()
  }
