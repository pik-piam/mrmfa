#' Convert clinker data from Andrew 2019.
#' @author Bennet Weiss
#' @param x Magpie object
convertAndrew2019 <- function(x) {
  # clinker data for a lot of countries is missing, those will default to NA
  no_remove_warning <- c("KSV")

  # pre-1800 data is not needed
  x <- x[, getYears(x, as.integer = TRUE) >= 1800, ]

  x["SRB", , ] <- x["SRB", , ] + toolNAreplace(x["KSV", , ])$x

  # Split USA (incl. Puerto Rico) into USA and PRI by GDP share.
  # USA includes PRI since version 210505, see changelog at https://zenodo.org/records/4738593 (accessed 2026-11-02).
  gdp <- calcOutput("CoGDP", years = getYears(x), aggregate = FALSE)[c("USA", "PRI"), , ]
  usa_pri <- toolAggregate(x["USA", , ], rel = data.frame(from = "USA", to = c("USA", "PRI")), weight = gdp)
  x <- mbind(x[getItems(x, dim = 1) != "USA", , ], usa_pri)

  x <- madrat::toolISOhistorical(x)
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  return(x)
}
