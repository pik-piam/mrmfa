#' Read clinker production data from Andrew's 2019 paper
#'
#' https://zenodo.org/records/20397304
#' Last version update 31.08.2026
#' Since version v260831, this source only contains clinker. Cement is in Andrew2019Cement.
#' Dataset update from:
#' Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928–2018.
#' Earth System Science Data 11, 1675–1710. https://doi.org/10.5194/essd-11-1675-2019.
#' @author Bennet Weiss
readAndrew2019 <- function() {
  version <- "v260830"
  path <- file.path(version, "2. annual_clinker_production.csv")
  data <- suppressMessages(readr::read_csv(path))
  # clean up data such that the country row becomes a column, too
  data_extracted <- tidyr::pivot_longer(data, -"Year", names_to = "region", values_to = "value")
  data_extracted <- dplyr::rename(data_extracted, "year" = "Year")
  x <- magclass::as.magpie(data_extracted, temporal = 1, spatial = 2)
  getNames(x) <- NULL
  return(x)
}
