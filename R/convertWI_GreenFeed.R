#' Convert Wuppertal Institute GreenFeed EoL parameters to remind-mfa categories
#'
#' @description
#' Map the raw Wuppertal Institute GreenFeed (GF) EoL rates onto the remind-mfa
#' dimensions and ISO country level:
#' \itemize{
#'   \item The 7 aggregate GF sectors are mapped to the 8 remind-mfa goods via
#'     \code{sectormappingWI_GreenFeed.csv} (GF has no data for
#'     \code{Textile sector} / \code{Industrial Machinery}).
#'   \item The 14 GF polymers are mapped to the remind-mfa material set via
#'     \code{polymermappingWI_GreenFeed.csv}.
#'   \item The 5 GF sub-regions are disaggregated to their EU member countries
#'     via \code{regionmappingWI_GreenFeed.csv}. As the GF values are intensive
#'     rates, each country inherits (a copy of) its sub-region's rate rather than
#'     a weight-split share.
#' }
#' Many-to-one sector/polymer collapses (e.g. \code{PS} + \code{PS-E}
#' \eqn{\rightarrow} \code{PS}, or the various engineering thermoplastics
#' \eqn{\rightarrow} \code{Other thermoplastics}) are combined as an unweighted
#' mean, since the GF files carry no sub-category tonnages to weight by.
#' Countries outside the covered EU sub-regions are filled with \code{0} (no GF
#' data); \code{\link{calcPlEoLGreenFeed}} sets their aggregation weight to 0 so
#' they do not dilute regional means.
#'
#' @param x magpie object with the raw GF EoL rates (see \code{\link{readWI_GreenFeed}}).
#' @return magpie object of the GF EoL rate mapped to remind-mfa sectors and
#'   polymers at ISO country resolution.
#' @author Leonie Schweiger
#' @seealso \code{\link{readWI_GreenFeed}}, \code{\link{calcPlEoLGreenFeed}}
#' @importFrom magclass getItems getItems<-
convertWI_GreenFeed <- function(x) {
  # ---------------------------------------------------------------------------
  # Map GF sectors (7 -> remind goods) and polymers (14 -> remind materials).
  # Many-to-one collapses are averaged (unweighted mean via a ones-weight).
  # ---------------------------------------------------------------------------
  sectorMap  <- toolGetMapping("sectormappingWI_GreenFeed.csv", type = "sectoral", where = "mrmfa")
  polymerMap <- toolGetMapping("polymermappingWI_GreenFeed.csv", type = "sectoral", where = "mrmfa")
  ones <- x
  ones[, , ] <- 1
  x <- toolAggregate(x, rel = sectorMap, dim = "sector", from = "Source", to = "Target", weight = ones)
  ones <- x
  ones[, , ] <- 1
  x <- toolAggregate(x, rel = polymerMap, dim = "polymer", from = "Source", to = "Target", weight = ones)

  # ---------------------------------------------------------------------------
  # Disaggregate GF sub-regions to EU member countries by copying the rate to
  # every member country, then fill the remaining countries with NA.
  # ---------------------------------------------------------------------------
  regionMap <- toolGetMapping("regionmappingWI_GreenFeed.csv", type = "regional", where = "mrmfa")
  xCountry <- toolAggregate(x, rel = regionMap, dim = "region", from = "GFRegion", to = "CountryCode", weight = NULL)
  xCountry <- toolCountryFill(xCountry, fill = 0, verbosity = 2)

  return(xCountry)
}
