#' Calculates the buildings floor area from EDGE-B
#'
#' @param scenario EDGE-B scenario, one of "SSP1", "SSP2", "SSP3", "SSP4", "SSP5"
#' @author Bennet Weiss
calcCeFloorspaceEDGEB <- function(scenario = "SSP2") {

  # Read data
  floorspace <- calcOutput("Floorspace", scenario = scenario, aggregate = FALSE)

  # Unit conversion from million m2 to m2
  floorspace <- floorspace * 1e6

  # Remove redundant dimensions (Scenario)
  floorspace <- dimReduce(floorspace)

  # Remove buildings total
  floorspace <- floorspace[,,(Variable = "buildings"), invert = TRUE]

  # enforce MFA naming convention
  getNames(floorspace) <- gsub("commercial", "Com", getNames(floorspace))
  getNames(floorspace) <- gsub("residential", "Res", getNames(floorspace))

  # TODO interpolate smoother than linear.
  year_range <- range(getYears(floorspace, as.integer = TRUE))
  target_years <- seq(year_range[1], year_range[2])
  floorspace_annual <- time_interpolate(dataset = floorspace,
                                        interpolated_year = target_years,
                                        integrate_interpolated_years = FALSE,
                                        extrapolation_type = "linear")

  # Output
  description <- "Floor area calculated from EDGE-B. Linearly interpolated."
  note <- "dimensions: (Time,Region,Stock Type,value)"
  output <- list(
    x = floorspace_annual,
    weight = NULL,
    unit = "m2",
    description = description,
    note = note
  )
}
