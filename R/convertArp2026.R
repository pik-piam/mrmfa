#' Convert Arp et al. 2026 material consumption to ISO country level
#'
#' @param x MagPIE object with Arp2026 data at IMAGE region resolution.
#' @return MagPIE object with the Arp2026 data disaggregated to ISO3 country
#' level, in Mt/yr.
#' @author Bennet Weiss
#' @seealso \code{\link{readArp2026}}
#' @importFrom magclass getYears getItems
convertArp2026 <- function(x) {
  map <- toolGetMapping("regionmapping_IMAGE_v3.4.csv", type = "regional", where = "mrmfa")

  if (!setequal(getItems(x, dim = 1), map$RegionCode)) {
    stop("IMAGE regions in Arp2026 do not match the RegionCode column of regionmapping_IMAGE_v3.4.csv.")
  }

  # SSP2 population as disaggregation weight
  pop <- calcOutput("CoPopulation", scenarios = "SSP2", aggregate = FALSE)
  pop <- pop[, getYears(x), ]

  # distribute each region's flows across its countries proportionally to population
  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "RegionCode", to = "CountryCode",
    weight = pop
  )

  return(x)
}
