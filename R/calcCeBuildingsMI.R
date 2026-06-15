#' Calculate Material Intensity (t/m2) of residential and commercial buildings.
#' Based on RASMI
#' @author Bennet Weiss
#' @param subtype Material subtype. Possible values are "concrete", "steel", "plastics".
calcCeBuildingsMI <- function(subtype = "concrete") {
  x <- readSource("RASMI", subtype)
  x <- x / 1000 # convert kg to t
  # placeholder MI of 1 for the "N/A" function/structure, whose inflow is determined directly
  x <- toolAddPlaceholder(x, "N/A.N/A", fill = 1)

  # use floor area for weight
  weight <- calcOutput("CeFloorspaceGEM", subtype = c("Function", "Structure"), aggregate = FALSE)
  weight <- toolAddPlaceholder(weight, "N/A.N/A", fill = 1)
  # align weight with x: drops GEM categories not present in RASMI (e.g. blank structure)
  weight <- weight[, , getItems(x, dim = 3)]
  # country without floorspace should still get MI if aggregated on country level
  weight[weight == 0] <- 1e-9
  description <- paste(
    "Material Intensity of buildings by stock type, function and structure.",
    "Based on RASMI.",
    "Fishman, T., Mastrucci, A., Peled, Y. et al.",
    "RASMI: Global ranges of building material intensities differentiated by region, structure, and function.",
    "Sci Data 11, 418 (2024). https://doi.org/10.1038/s41597-024-03190-7"
  )
  note <- "dimensions: (Region,Function,Structure,value)"
  output <- list(
    x = x,
    weight = weight,
    unit = "t/m2",
    description = description,
    note = note
  )
  return(output)
}
