#' Convert World Steel Database
#' @description Convert data from WorldSteelAssociation Database
#' @author Merlin Jo Hosak
#' @param x Magpie object
#' @param subtype TODOMERLIN: document
convertWorldSteelDatabase <- function(x, subtype = "production") {
  x <- toolSplitYugBlx(x)
  x <- x[!is.na(getItems(x, dim = 1)), ] # remove rows with NA in country_name column
  x <- toolCountryFill(x, verbosity = 2)

  return(x)
}

toolSplitYugBlx <- function(x) {
  # Add historical mapping for Yugoslavia with last year being 2005
  # instead of 1991 as there is some aggregated data in this dataset
  # for the years 2002-2005 for Yugoslavia. Similar for Belgium and Luxembourg.

  yugMapping <- toolMakeHistoricalMap("YUG",
    c("SRB", "MNE", "SVN", "HRV", "MKD", "BIH"),
    lastYear = "y2005"
  )
  blxMapping <- toolMakeHistoricalMap("BLX",
    c("BEL", "LUX"),
    lastYear = "y2003"
  )

  historicalMapping <- rbind(yugMapping, blxMapping)

  newCountries <- historicalMapping$toISO
  missingCountries <- setdiff(newCountries, getItems(x, dim = 1))
  x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

  x <- toolISOhistorical(x, overwrite = TRUE, mapping = historicalMapping) %>% suppressWarnings()

  return(x)
}

toolMakeHistoricalMap <- function(joinRegion, newCountries, lastYear) {
  mapping <- list(
    fromISO = rep(joinRegion, length(newCountries)),
    toISO = newCountries,
    lastYear = rep(lastYear, length(newCountries))
  ) %>% as.data.frame()
  return(mapping)
}
