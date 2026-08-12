#' Convert Ren et al. (2025) China plastic flows to remind-mfa categories
#'
#' @description
#' Map the raw Ren et al. (2025) flows onto the remind-mfa categories. 
#' The 20 source polymers are mapped to the base polymer set (\code{polymermappingRen2025.csv}), 
#' keeping \code{PA} (polyamide) as its own category so that \code{\link{calcPlRen2025}} 
#' can later split off the polyamide fibre share. The nine end-use sectors are mapped to the eight
#' remind-mfa goods (\code{sectormappingRen2025.csv}) and the disposal categories
#' are renamed to the mrmfa vocabulary (\code{Recycled}, \code{Incinerated},
#' \code{Landfilled}, \code{Untreated}). Countries other than China are filled with 0.
#'
#' @param x magpie object with the raw Ren et al. (2025) China plastic flows.
#' @inherit readRen2025 return
#' @author Leonie Schweiger
#' @seealso \code{\link{readRen2025}}
#' @importFrom magclass getItems getItems<-
convertRen2025 <- function(x) {
  # ---------------------------------------------------------------------------
  # Map polymers (20 -> base set, PA kept distinct) and sectors (9 -> 8 goods)
  # ---------------------------------------------------------------------------
  polymerMap <- toolGetMapping("polymermappingRen2025.csv", type = "sectoral", where = "mrmfa")
  sectorMap  <- toolGetMapping("sectormappingRen2025.csv", type = "sectoral", where = "mrmfa")

  x <- toolAggregate(x, rel = polymerMap, dim = "polymer", from = "Source", to = "Target")
  x <- toolAggregate(x, rel = sectorMap, dim = "sector", from = "Source", to = "Target")

  # ---------------------------------------------------------------------------
  # Rename disposal categories to the mrmfa vocabulary (the "/" placeholder stays)
  # ---------------------------------------------------------------------------
  disposalMap <- c(
    "/"            = "/",
    "Recycling"    = "Recycled",
    "Incineration" = "Incinerated",
    "Landfill"     = "Landfilled",
    "Untreated"    = "Untreated"
  )
  getItems(x, dim = "disposal") <- unname(disposalMap[getItems(x, dim = "disposal")])

  # ---------------------------------------------------------------------------
  # Complete the country set (all countries except China are 0)
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
