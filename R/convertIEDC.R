#' @author Falk Benke
#' @inherit readIEDC
#' @param x MagPIE object
convertIEDC <- function(x, subtype) {
  countries <- getItems(x, dim = 1)

  # replace underscores with dots (special character in magclass)
  countries <- gsub("_", ".", countries)
  # drop hyphens
  countries <- gsub("-", " ", countries)

  ignore <- toolGetMapping("MFA_ignore_regions.csv", where = "mrmfa")$reg

  m <- toolGetMapping("MFA_rename_regions.csv", where = "mrmfa")
  additionalIsoMappings <- m$to
  names(additionalIsoMappings) <- m$from

  getItems(x, dim = 1) <- toolCountry2isocode(countries,
    mapping = additionalIsoMappings,
    ignoreCountries = ignore, warn = TRUE
  )

  # remove new rows with NA in country_name column (that were ignored)
  x <- x[!is.na(getItems(x, dim = 1)), ]

  # disaggregate no longer existing countries ----

  # use WorldSteel data as reference
  ws <- readSource("WorldSteelDatabase", subtype = subtype)
  ws[is.na(ws)] <- 0
  ws <- ws[, getYears(ws, as.integer = TRUE) >= 2009, ]

  x <- toolMerge2D(x, ws)

  # create custom iso mapping matching the data
  histMapping <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
    filter(
      .data$fromISO %in% c("SUN", "CSK", "ANT", "YUG"),
      !.data$toISO %in% c("SCG")
    )

  # all former regions have data until 2008
  histMapping$lastYear <- "y2009"

  # directly transform YUG -> SRB
  # - not via SCG as data contains different lastYear definition than standard mapping
  # - do not distribute anything to MNE, unlike in standard mapping
  scg <- data.frame(
    fromISO = "YUG",
    toISO = "SRB",
    lastYear = "y2009"
  )

  blx <- data.frame(
    fromISO = "BLX",
    toISO = c("BEL", "LUX"),
    lastYear = "y2009"
  )

  histMapping <- rbind(histMapping, scg, blx)

  x <- toolISOhistorical(x, overwrite = TRUE, mapping = histMapping) %>%
    suppressSpecificWarnings("Weight in toolISOhistorical contained NAs. Set NAs to 0!")

  x <- toolCountryFill(x, verbosity = 2)

  # drop reference data again
  x <- x[, getYears(x, as.integer = TRUE) < 2009, , invert = TRUE]

  return(x)
}
