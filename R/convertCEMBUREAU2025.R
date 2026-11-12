#' Convert cement end-use shares from CEMBUREAU (2025)
#'
#' Normalises the shares after dropping the category "Unknown". The EU27 aggregate is assigned to
#' its 27 member states (EU composition since 2020). Other countries are filled with NA.
#'
#' @author Bennet Weiss
#' @param x magpie object from \code{\link{readCEMBUREAU2025}}.
convertCEMBUREAU2025 <- function(x) {
  x <- toolCeNormalizeEndUseSplit(x)

  eu27 <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
    "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )
  xOut <- new.magpie(eu27, getYears(x), getNames(x), sets = getSets(x))
  xOut[, , ] <- x["EU27", , ]
  return(toolCountryFill(xOut, fill = NA, verbosity = 2))
}
