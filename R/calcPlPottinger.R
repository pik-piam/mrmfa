#' Pottinger 2024 plastics material flows in IAMC variables
#'
#' @description
#' Return the Business-as-Usual plastics material-flow data from Pottinger et al.
#' (2024) at ISO3 country level, with the source flow columns renamed to IAMC
#' variables under the \code{Materials|Plastics|...} hierarchy. All flows are in
#' Mt/yr and are absolute quantities, so \code{weight = NULL} (they are summed
#' when aggregated to regions).
#'
#' @author Leonie Schweiger
#' @return List with a MagPIE object of plastics flows (Mt/yr) in IAMC variables
#' and metadata in calcOutput format.
#' @seealso \code{\link{readPottinger2024}}
#' @importFrom madrat readSource
#' @importFrom magclass getItems getItems<-
calcPlPottinger <- function() {
  x <- readSource("Pottinger2024")

  # map raw source flow columns to IAMC variables (Materials|Plastics| hierarchy)
  iamc <- c(
    totalConsumptionMt                  = "Material Demand|Chemicals|Plastics",
    primaryProductionMt                 = "Production|Chemicals|Plastics|Primary",
    secondaryProductionMt               = "Production|Chemicals|Plastics|Secondary"
  )

  # keep only the mapped variables
  x <- x[, , names(iamc)]

  # apply the IAMC names
  getItems(x, dim = 3) <- as.vector(iamc)

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt/yr",
    description = paste(
      "Plastics material-flow Business-as-Usual pathway 2011-2050 from",
      "Pottinger et al. (2024), in IAMC variables"
    ),
    note = "Disaggregated from 4 Pottinger regions to ISO3 via population weighting"
  ))
}
