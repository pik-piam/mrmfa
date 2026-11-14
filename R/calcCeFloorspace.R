#' Calculates the buildings floor area
#'
#' Floor space per capita of the EDGE-B regions is disaggregated to countries with weights
#' from the EDGE-B floor space regression (see \link{calcCeFloorspaceWeightsEDGEB}).
#' Absolute floor space is obtained by multiplying with the MFA population (see \link{calcCoPopulation}).
#'
#' @param perCapita Logical. If TRUE, floor space is returned per capita.
#' @param scenarios EDGE-B scenarios (character vector or string). Available: "SSP1", "SSP2", "SSP3", "SSP4", "SSP5".
#' @param collapse Logical. If TRUE, redundant dimensions (e.g. scenario if only one requested) are removed.
#' @param smooth Logical. If TRUE, data is smoothed using spline interpolation
#' (see \link{toolHistoricallyConsistentSmoothing}); smoothed historical values are identical across scenarios.
#' @param dof Integer. Degrees of freedom for spline interpolation.
#' @author Bennet Weiss
calcCeFloorspace <- function(perCapita = FALSE, scenarios = "SSP2", collapse = TRUE, smooth = FALSE, dof = 8) {
  # Read floor space per capita (m2/cap) from EDGE-Buildings on its native regions
  floorspace_pc <- calcOutput("Floorspace", scenario = scenarios, perCap = TRUE,
                              regionmapping = "regionmappingEDGE.csv", aggregate = "RegionCodeEUR_ETP")

  # Remove buildings total
  floorspace_pc <- floorspace_pc[, , "buildings", invert = TRUE]

  # Disaggregate to countries with weights from the EDGE-B floor space regression
  mapping <- toolGetMapping("regionmappingEDGE.csv", type = "regional", where = "mappingfolder")
  scens <- getItems(floorspace_pc, dim = "scenario")
  floorspace_years <- getYears(floorspace_pc)
  weights <- calcOutput("CeFloorspaceWeightsEDGEB", scenarios = scens, aggregate = FALSE)
  pop <- calcOutput("CoPopulation", scenarios = scens, collapse = FALSE, aggregate = FALSE)[, floorspace_years]
  pop_regional <- toolAggregate(pop, rel = mapping, from = "CountryCode", to = "RegionCodeEUR_ETP")
  floorspace <- toolAggregate(floorspace_pc * pop_regional, rel = mapping, weight = weights[, floorspace_years],
                              from = "RegionCodeEUR_ETP", to = "CountryCode")
  floorspace_pc <- floorspace / pop

  # enforce MFA naming convention
  getNames(floorspace_pc) <- gsub("commercial", "Commercial", getNames(floorspace_pc))
  getNames(floorspace_pc) <- gsub("residential", "Residential", getNames(floorspace_pc))

  floorspace_years <- getYears(floorspace_pc, as.integer = TRUE)
  # smooth if requested
  if (smooth) {
    # smooth data while keeping the historical period identical across scenarios
    refScenario <- if ("SSP2" %in% scens) "SSP2" else scens[1]
    floorspace_pc <- toolHistoricallyConsistentSmoothing(
      floorspace_pc,
      refScenario = refScenario,
      dof = dof,
      # data beyond 2100 is not smoothed due to low data quality, but joined continuously
      lastSmoothedYear = 2100
    )
  }

  # interpolate to yearly data
  floorspace_pc <- toolInterpolate(
    floorspace_pc,
    years = seq(floorspace_years[1], floorspace_years[length(floorspace_years)], 1),
    type = "monotone"
  )

  # convert to absolute floor space (m2) with the MFA population unless per capita is requested
  pop <- calcOutput("CoPopulation", scenarios = scens, collapse = FALSE, smooth = smooth, dof = dof,
                    aggregate = FALSE)[, getYears(floorspace_pc)]
  if (perCapita) {
    x <- floorspace_pc
    unit <- "m2/cap"
    weight <- pop
  } else {
    x <- floorspace_pc * pop
    unit <- "m2"
    weight <- NULL
  }

  # Remove redundant dimensions (e.g. scenario, if only one is given)
  if (collapse) {
    x <- collapseDim(x)
    if (!is.null(weight)) {
      weight <- collapseDim(weight)
    }
  }

  # Output
  smooth_suffix <- if (smooth) " and spline smoothed." else "."
  description <- paste0(
    "Floor area ", if (perCapita) "per capita " else "", "calculated from EDGE-B. ",
    "Interpolated using monotone cubic splines",
    smooth_suffix
  )
  scenario_note <- if (length(scenarios) == 1 && collapse) "" else ",Driver Scenario"
  note <- paste0("dimensions: (Time,Region", scenario_note, ",Common End Use,value)")
  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
