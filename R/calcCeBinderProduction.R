#' Calculates global cement production as from Andrew's 2019 paper.
#' @author Bennet Weiss
#' @param subtype Material subtype. Can be "cement or "clinker".
calcCeBinderProduction <- function(subtype) {
  x <- readSource("Andrew2019", subtype)
  if (subtype == "cement") {
    x <- toolInterpolate(x, type = "spline", extrapolate = FALSE, maxgap = 10)

    # Backcast missing data in early 20th century using regional GDP and US cement intensity
    gdp <- calcOutput("CoGDP", years = getYears(x), aggregate = FALSE)
    # Complete data available for US: t cement production per unit of GDP
    us_cement_intensity <- x["USA", ] / gdp["USA", ]
    getItems(us_cement_intensity, dim = 1) <- "GLO"
    reference_cement_production <- us_cement_intensity * gdp
    x <- toolBackcastByReference(x, reference_cement_production)

  } else if (subtype == "clinker") {
    x[is.na(x)] <- 0
  } else {
    stop("Invalid subtype. Please choose either 'cement' or 'clinker'.")
  }

  x <- x * 1e3 # convert to tonnes
  unit <- "tonnes (t)"
  description <- paste(
    "Annual ", subtype, " production as from",
    "Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928-2018.",
    "Earth System Science Data 11, 1675-1710. https://doi.org/10.5194/essd-11-1675-2019.",
    "Data reported on https://zenodo.org/records/11207133.",
    "Accessed: 24.02.2025."
  )
  note <- "dimensions: (Historic Time,Region,value)"
  output <- list(x = x, weight = NULL, unit = unit, description = description, note = note)
  return(output)
}
