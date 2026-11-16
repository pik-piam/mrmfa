#' Arp et al. 2026 steel or cement demand in IAMC variables
#'
#' @description
#' Return total steel or cement consumption from Arp et al. (2026, IMAGE) at ISO3
#' country level for all four SSP2 scenarios, summed over the six end-use sectors
#' (buildings, vehicles, electricity generation, grid, storage, rest).
#'
#' @param subtype Character, material: \code{"steel"} or \code{"cement"}.
#' @author Bennet Weiss
#' @return List with a MagPIE object of steel or cement demand (Mt/yr) in IAMC
#' variables and metadata in calcOutput format.
#' @seealso \code{\link{readArp2026}}, \code{\link{convertArp2026}}
#' @importFrom magclass dimSums add_dimension
calcCoArp2026 <- function(subtype) {
  variables <- c(
    steel = "Material Demand|Iron and Steel|Steel",
    cement = "Material Demand|Non-Metallic Minerals|Cement"
  )
  if (!subtype %in% names(variables)) {
    stop("Unknown subtype '", subtype, "'. Use 'steel' or 'cement'.")
  }

  x <- readSource("Arp2026", subtype = subtype)

  # sum over end-use sectors
  x <- dimSums(x, dim = "Sector")
  x <- add_dimension(x, dim = 3.2, add = "Variable", nm = variables[[subtype]])

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt/yr",
    description = paste(
      "Total", subtype, "consumption 2020-2100 from Arp et al. (2026, IMAGE),",
      "all SSP2 scenarios, summed over end-use sectors."
    ),
    note = "Disaggregated from 26 IMAGE regions to ISO3 via SSP2 population weighting"
  ))
}
