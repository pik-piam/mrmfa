#' Calculates global cement production as from Andrew's 2019 paper.
#' @author Bennet Weiss
#' @param subtype Material subtype. Possible values are "concrete", "steel", "plastics".
calcBuildingsMI <- function(subtype = "concrete") {
  x <- readSource("RASMI", subtype)
  x <- x / 1000 # convert kg to t

  # use floor area for weight
  weight <- calcOutput("BuildingFloorArea", subtype = c("Function", "Structure"), aggregate = FALSE)
  weight[weight == 0] <- 1e-9 # hack to avoid weight sum of 0 in some cases (TODO is there a better way?)
  description <- paste(
    "Material Intensity of buildings by stock type, function and structure.",
    "Based on RASMI.",
    "Fishman, T., Mastrucci, A., Peled, Y. et al.",
    "RASMI: Global ranges of building material intensities differentiated by region, structure, and function.",
    "Sci Data 11, 418 (2024). https://doi.org/10.1038/s41597-024-03190-7"
  )
  output <- list(
    x = x,
    weight = weight,
    unit = "t/m2",
    description = description
  )
}
