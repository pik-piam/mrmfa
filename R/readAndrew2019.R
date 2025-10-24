#' Read Data from Andrew's 2019 paper
#'
#' https://zenodo.org/records/14931651
#' Last version update 26.02.2025
#' Dataset update from:
#' Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928–2018.
#' Earth System Science Data 11, 1675–1710. https://doi.org/10.5194/essd-11-1675-2019.
#' @author Bennet Weiss
#' @param subtype Material subtype. Can be "cement" or "clinker".
readAndrew2019 <- function(subtype) {
  folder <- "v2"
  if (subtype == "cement") {
    path <- file.path(folder, "1. annual_cement_production.csv")
  } else if (subtype == "clinker") {
    path <- file.path(folder, "2. annual_clinker_production.csv")
  } else {
    stop("Invalid subtype. Choose either 'cement' or 'clinker'.")
  }
  data <- suppressMessages(read_csv(path))
  # clean up data such that the country row becomes a column, too
  data_extracted <- tidyr::pivot_longer(data, -"Year", names_to = "region", values_to = "value")
  x <- magclass::as.magpie(data_extracted, spatial = 2)
  getNames(x) <- NULL
  return(x)
}
