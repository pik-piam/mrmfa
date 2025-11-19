#' Calculates the buildings floor area from EDGE-B
#'
#' @param scenario EDGE-B scenario, one of "SSP1", "SSP2", "SSP3", "SSP4", "SSP5"
#' @author Bennet Weiss
calcCeEDGEBFloorSpace <- function(scenario = "SSP2") {

  # Read data
  floorspace <- calcOutput("Floorspace", scenario = scenario)

  # Unit conversion from million m2 to m2
  floorspace <- floorspace * 1e6

  # Remove redundant dimensions (Scenario)
  floorspace <- dimReduce(floorspace)

  # Remove buildings total
  floorspace <- floorspace[,,(Variable = "buildings"), invert = TRUE]

  # TODO interpolate missing years.

  # Output
  description <- "Floor area calculated from EDGE-B."
  note <- "dimensions: (Time,Region,Stock Type,value)"
  output <- list(x = floorspace, weight = NULL, unit = "m2", description = description, note = note)
}
