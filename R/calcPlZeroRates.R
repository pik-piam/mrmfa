#' Dummy function to create magpie object with rates that are historically 0.
#' E.g. for chemical recycling, bio-based production and DAC based production
#'
#' @author Leonie Schweiger
#'
calcPlZeroRates <- function() {
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  regions <- unique(region_map$CountryCode)
  years <- 1950:2019

  x <- new.magpie(
    cells_and_regions = regions,
    years = years,
    names = NULL,
    fill = 0,
  )
  weight <- x
  weight[, ] <- 1
  description <- paste0("Rates that are historically 0, e.g. chemical recycling rate, ",
                        "bio-based & DAC production rate and emission capture rate")
  return(list(
    x           = x,
    weight      = weight,
    unit        = "share",
    description = description,
    note        = "dimensions: (Historic Time,Region,value)"
  ))
}
