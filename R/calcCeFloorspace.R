#' Calculates the floorspace of residential (Res) and commercial (Com) buildings.
#' Based on floorspace data from EDGE-B.
#'
#' @param scenario EDGE-B scenario, one of "SSP1", "SSP2", "SSP3", "SSP4", "SSP5".
#' @author Bennet Weiss
calcCeFloorspace <- function(scenario = "SSP2") {
  floorspace <- calcOutput("CeFloorspaceEDGEB", scenario = scenario, aggregate = FALSE)

  # Output
  description <- paste(
    "Floor area calculated from linearly interpolated EDGE-B data."
  )
  note <- "dimensions: (Time,Region,Stock Type,value)"
  output <- list(
    x = floorspace,
    weight = NULL,
    unit = "m2",
    description = description,
    note = note
  )
  return(output)
}
