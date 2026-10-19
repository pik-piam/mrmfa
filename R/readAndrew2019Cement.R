#' Read cement production data from Andrew's 2019 paper
#'
#' https://zenodo.org/records/22207852
#' Last version update 31.08.2026
#' Cement data, reported separately from the clinker data (Andrew2019) since version v260831.
#'
#' Dataset update from:
#' Andrew, R. (2026). Annual global cement production data (Version 260831) [Data set]. Zenodo.
#' https://doi.org/10.5281/zenodo.22207852
#' @author Bennet Weiss
readAndrew2019Cement <- function() {
  version <- "v260831"
  path <- file.path(version, "annual_cement_production.csv")
  data <- suppressMessages(readr::read_csv(path))
  # clean up data such that the country row becomes a column, too
  data_extracted <- tidyr::pivot_longer(data, -"Year", names_to = "region", values_to = "value")
  data_extracted <- dplyr::rename(data_extracted, "year" = "Year")
  x <- magclass::as.magpie(data_extracted, temporal = 1, spatial = 2)
  getNames(x) <- NULL
  return(x)
}
