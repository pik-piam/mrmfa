#' Calculates the floorspace of residential (Res) and commercial (Com) buildings.
#' Based on floorspace data from EDGE-B.
#' Corrected by region-specific calibration using EUBUCCO and GEM if correct = TRUE.
#'
#' @param scenario EDGE-B scenario, one of "SSP1", "SSP2", "SSP3", "SSP4", "SSP5".
#' @param correct Bool, whether to apply the correction factor (default: TRUE).
#' @author Bennet Weiss
calcCeFloorspace <- function(scenario = "SSP2", correct = TRUE) {
  floorspace_edgeb <- calcOutput("CeFloorspaceEDGEB", scenario = scenario, aggregate = FALSE)
  floorspace_correction <- calcOutput("CeFloorspaceCorrectionFactor", aggregate = FALSE)

  if (correct) {
    floorspace_out <- floorspace_edgeb * floorspace_correction
  } else {
    floorspace_out <- floorspace_edgeb
  }

  # Output
  description <- paste(
    "Floor area calculated from linearly interpolated EDGE-B data.",
    "Corrected by region-specific calibration using EUBUCCO and GEM."
  )
  note <- "dimensions: (Time,Region,Stock Type,value)"
  output <- list(
    x = floorspace_out,
    weight = NULL,
    unit = "m2",
    description = description,
    note = note
  )
  return(output)
}
