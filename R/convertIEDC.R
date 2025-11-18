#' @author Falk Benke
convertIEDC <- function(x, subtype) {
  countries <- getItems(x, dim = 1)

  # replace underscores with dots (special character in magclass)
  countries <- gsub("_", ".", countries)
  # drop hyphens and slashes
  countries <- gsub("-|–|/", " ", countries)
  # replace two or more whitespaces by one
  countries <- gsub("  +", " ", countries)
  # remove appended info in round brackets, e.g. "Taiwan (R.O.C.)" -> "Taiwan"
  countries <- gsub(" *\\(.*\\) *$", "", countries)
  # "German Dem . Rep ." -> "German Dem. Rep.",
  countries <- gsub(" \\.", "\\.", countries)
  # "Taiwan , China" -> "Taiwan, China
  countries <- gsub(" ,", ",", countries)

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

  # create custom iso mapping matching the data
  mapping <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
    filter(
      .data$fromISO %in% c("SUN", "CSK", "ANT", "YUG"),
      !.data$toISO %in% c("SCG")
    )

  # all former regions have data until 2008
  mapping$lastYear <- "y2008"

  # directly transform YUG -> SRB
  # - not via SCG as data contains different lastYear definition than standard mapping
  # - do not distribute anything to MNE, unlike in standard mapping
  scg <- data.frame(
    fromISO = "YUG",
    toISO = "SRB",
    lastYear = "y2008"
  )

  blx <- data.frame(
    fromISO = "BLX",
    toISO = c("BEL", "LUX"),
    lastYear = "y2008"
  )

  # TODO: check if there is a problem with the change: https://github.com/leonieschweiger/mrmfa/blob/steel_updates/R/calcStPigIronPreliminaryData.R#L66
  # before: lastYear was 2009 of WorldSteelDatabase data
  # now: lastYear is 2008 of IEDS data

  mapping <- rbind(mapping, scg, blx)

  # add regions not present in the magpie object yet needed for toolISOhistorical to work

  missingCountries <- mapping %>%
    dplyr::pull("toISO") %>%
    setdiff(getItems(x, dim = 1))
  x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

  # TODO: workaround to make toolISOhistorical not crash
  x <- add_columns(x, addnm = "y2009", dim = 2, fill = NA)

  y <- toolISOhistorical(x, overwrite = TRUE, mapping = mapping)

  x <- toolCountryFill(y, verbosity = 2)

  # TODO: workaround to make toolISOhistorical not crash
  x <- x[, 2009, , invert = T]

  return(x)
}
