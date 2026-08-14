#' Calculate sector- and polymer-resolved EU plastics EoL rates (GreenFeed)
#'
#' @description
#' Derive sector- and polymer-differentiated end-of-life (EoL) rates for the EU
#' from the Wuppertal Institute "GreenFeed" (GF) baseline dataset
#' (\code{\link{readWI_GreenFeed}}, \code{\link{convertWI_GreenFeed}}).
#' Only EU countries carry data; other countries are \code{NA}.
#' Select the rate via \code{subtype}:
#' \describe{
#'   \item{\code{"Collection"}}{Share of generated plastic waste that is
#'     collected (polymer-independent in the source, reported across all
#'     polymers).}
#'   \item{\code{"MechanicalRecycling"}}{Share of collected waste sorted into
#'     mechanical recycling.}
#'   \item{\code{"Incineration"}}{Share of collected waste sorted into
#'     incineration.}
#'   \item{\code{"RecyclingYield"}}{Conversion rate of sorted mechanical-recycling
#'     waste into secondary raw material.}
#' }
#'
#' @param subtype One of \code{"Collection"}, \code{"MechanicalRecycling"},
#'   \code{"Incineration"}, \code{"RecyclingYield"}.
#' @return A list in \code{\link[madrat]{calcOutput}} format. Regional
#'   aggregation uses population as weight.
#' @author Leonie Schweiger
#' @seealso \code{\link{readWI_GreenFeed}}, \code{\link{convertWI_GreenFeed}},
#'   \code{\link{calcPlEoL_shares}}
#' @examples
#' \dontrun{
#' a <- calcOutput("PlEoLGreenFeed", subtype = "MechanicalRecycling")
#' }
#' @importFrom magclass mselect collapseDim getYears time_interpolate
calcPlEoLGreenFeed <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Select the requested rate from the GF datasets
  # ---------------------------------------------------------------------------
  x <- switch(subtype,
    "Collection" = readSource("WI_GreenFeed", subtype = "CollectionRate"),
    "MechanicalRecycling" = collapseDim(
      mselect(readSource("WI_GreenFeed", subtype = "SortingRate"), waste = "Mechanical recycling"),
      dim = "waste"
    ),
    "Incineration" = collapseDim(
      mselect(readSource("WI_GreenFeed", subtype = "SortingRate"), waste = "Incineration"),
      dim = "waste"
    ),
    "RecyclingYield" = collapseDim(
      readSource("WI_GreenFeed", subtype = "RecyclingYield"), dim = "waste"
    ),
    stop(
      "Invalid subtype '", subtype, "' -- supported: Collection, ",
      "MechanicalRecycling, Incineration, RecyclingYield"
    )
  )

  # ---------------------------------------------------------------------------
  # Population weight for regional aggregation, broadcast across sector/polymer.
  # Only the GF-covered EU countries get weight; all others get 0 so the filled
  # zeros do not dilute regional means.
  # ---------------------------------------------------------------------------
  pop <- calcOutput("CoPopulation", scenarios = "SSP2", aggregate = FALSE)
  pop <- pop[, getYears(x), ]

  gfCountries <- toolGetMapping("regionmappingWI_GreenFeed.csv",
    type = "regional", where = "mrmfa"
  )$CountryCode

  weight <- x
  weight[, , ] <- 0
  weight[gfCountries, , ] <- 1
  weight <- weight * pop

  return(list(
    x            = x,
    weight       = weight,
    unit         = "ratio",
    description  = paste(
      "Sector- and polymer-resolved EU plastics end-of-life", subtype,
      "rate from the Wuppertal Institute GreenFeed baseline dataset."
    ),
    note         = "dimensions: (Time, Region, Good, Material, value)",
    min          = 0,
    max          = 1
  ))
}
