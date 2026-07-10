#' Calculates the buildings floor area from EDGE-B
#'
#' @param scenarios EDGE-B scenarios (character vector or string). Available: "SSP1", "SSP2", "SSP3", "SSP4", "SSP5".
#' @param collapse Logical. If TRUE, redundant dimensions (e.g. scenario if only one requested) are removed.
#' @param smooth Logical. If TRUE, data is smoothed using spline interpolation
#' (see \link{toolHistoricallyConsistentSmoothing}); smoothed historical values are identical across scenarios.
#' @param dof Integer. Degrees of freedom for spline interpolation.
#' @author Bennet Weiss
calcCeFloorspaceEDGEB <- function(scenarios = "SSP2", collapse = TRUE, smooth = FALSE, dof = 8) {
  # Read data from EDGE-Buildings
  floorspace <- calcOutput("Floorspace", scenario = scenarios, aggregate = FALSE)

  # Unit conversion from million m2 to m2
  floorspace <- floorspace * 1e6

  # Remove buildings total
  floorspace <- floorspace[, , "buildings", invert = TRUE]

  # enforce MFA naming convention
  getNames(floorspace) <- gsub("commercial", "Com", getNames(floorspace))
  getNames(floorspace) <- gsub("residential", "Res", getNames(floorspace))

  floorspace_years <- getYears(floorspace, as.integer = TRUE)
  # smooth if requested
  if (smooth) {
    # smooth data while keeping the historical period identical across scenarios
    # exclude data beyond 2100 from smoothing due to low data quality
    years <- floorspace_years[floorspace_years <= 2100]
    scens <- getItems(floorspace, dim = "scenario")
    refScenario <- if ("SSP2" %in% scens) "SSP2" else scens[1]
    floorspace[, years] <- toolHistoricallyConsistentSmoothing(
      floorspace[, years],
      refScenario = refScenario,
      dof = dof
    )
  }

  # Remove redundant dimensions (e.g. scenario, if only one is given)
  if (collapse) {
    floorspace <- collapseDim(floorspace)
  }

  # interpolate to yearly data
  floorspace <- toolInterpolate(
    floorspace,
    years = seq(floorspace_years[1], floorspace_years[length(floorspace_years)], 1),
    type = "monotone"
  )

  # Output
  smooth_suffix <- if (smooth) " and spline smoothed." else "."
  description <- paste0(
    "Floor area calculated from EDGE-B. ",
    "Interpolated using monotone cubic splines",
    smooth_suffix
  )
  scenario_note <- if (length(scenarios) == 1 && collapse) "" else ",Driver Scenario"
  note <- paste0("dimensions: (Time,Region", scenario_note, ",Stock Type,value)")
  output <- list(
    x = floorspace,
    weight = NULL,
    unit = "m2",
    description = description,
    note = note
  )
  return(output)
}
