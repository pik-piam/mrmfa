#' Calculates global clinker production as from Andrew's 2019 paper.
#' @author Bennet Weiss
calcCeClinkerProduction <- function() {
  x <- readSource("Andrew2019")

  # convert to tonnes
  x <- x * 1e3

  # same time range as cement production
  cement_years <- getYears(calcOutput("CeCementProduction", aggregate = FALSE))
  missing_years <- setdiff(cement_years, getYears(x))
  if (length(missing_years) > 0) {
    stop("Clinker data does not cover all cement production years. Missing: ", paste(missing_years, collapse = ", "))
  }
  x <- x[, cement_years, ]

  unit <- "tonnes (t)"
  description <- paste(
    "Annual clinker production as from",
    "Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928-2018.",
    "Earth System Science Data 11, 1675-1710. https://doi.org/10.5194/essd-11-1675-2019.",
    "Data reported on https://zenodo.org/records/20397304."
  )
  note <- "dimensions: (Historical Time,Region,value)"
  output <- list(x = x, weight = NULL, unit = unit, description = description, note = note)
  return(output)
}
