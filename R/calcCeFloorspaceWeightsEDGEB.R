#' Weights for the disaggregation of EDGE-B floor space to countries
#'
#' Floor space per capita is assumed to follow the EDGE-B floor space regression
#' \code{log(m2cap) = c + a * log(gdppop) + b * log(density)} (residential) and
#' \code{log(m2cap) = c + a * log(gdppop)} (commercial), with the global coefficients
#' from \link[mredgebuildings]{calcFloorspacePast}. The weight is population times
#' \code{gdppop^a * density^b}; the intercept and unit factors cancel out in the
#' disaggregation. Non-finite population densities (no surface area) are replaced
#' by the global mean as in \link[mredgebuildings]{calcDensity}.
#'
#' @param scenarios population and GDP scenarios (character vector or string).
#' @author Bennet Weiss
calcCeFloorspaceWeightsEDGEB <- function(scenarios = "SSP2") {
  pop <- calcOutput("CoPopulation", scenarios = scenarios, collapse = FALSE, aggregate = FALSE)
  gdppop <- calcOutput("CoGDP", perCapita = TRUE, scenarios = scenarios, collapse = FALSE, aggregate = FALSE)

  # population density
  surface <- collapseDim(calcOutput("Surface", aggregate = FALSE), dim = c(2, 3))
  density <- pop / surface
  density[!is.finite(density)] <- mean(density[is.finite(density)])

  # global regression coefficients, missing coefficients (density for commercial) are zero
  coefs <- calcOutput("FloorspacePast", returnPars = TRUE, aggregate = FALSE)
  getCoef <- function(term, sector) {
    name <- paste0(term, "_", sector)
    if (name %in% getNames(coefs)) as.numeric(coefs[, , name]) else 0
  }

  # weight = population * gdppop^a * density^b
  calcWeights <- function(sector) {
    gdppopExponent <- getCoef("log(gdppop)", sector)
    densityExponent <- getCoef("log(density)", sector)
    sectorWeights <- pop * gdppop^gdppopExponent * density^densityExponent
    add_dimension(sectorWeights, dim = 3.2, add = "variable", nm = sector)
  }

  weights <- mbind(calcWeights("residential"), calcWeights("commercial"))
  getSets(weights) <- c("region", "year", "scenario", "variable")

  return(list(
    x = weights,
    weight = NULL,
    unit = "1",
    description = "Weights for the disaggregation of EDGE-B floor space to countries."
  ))
}
