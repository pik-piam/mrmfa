# change to ISO country codes
# TODO unify toolCountry2isocode handling in this function and consider moving it to convert
# altogether
toolFoo <- function(x) {
  # clean up region names ----
  # replace _ by dot
  regionNames <- gsub("_", "\\.", getItems(x, dim = 1))

  # replace two whitespaces by one
  regionNames <- gsub("  ", " ", regionNames)

  # remove appended info in round brackets, e.g. "Taiwan (R.O.C.)" -> "Taiwan"
  regionNames <- gsub(" *\\(.*\\) *$", "", regionNames)

  # "German Dem . Rep ." -> "German Dem. Rep."
  regionNames <- gsub(" \\.", "\\.", regionNames)

  # "Taiwan , China" -> "Taiwan, China
  regionNames <- gsub(" ,", ",", regionNames)

  # for now, we store all additional mappings in the mfa package
  # as a next step, move universal mappings to country2iso.csv in madrat
  m <- read.csv2(system.file("extdata", "MFA_rename_regions.csv", package = "mrmfa"), header = TRUE)
  additionalIsoMappings <- m$to
  names(additionalIsoMappings) <- m$from

  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"), header = TRUE)$reg

  getItems(x, dim = 1) <- toolCountry2isocode(regionNames,
    ignoreCountries = ignore,
    mapping = additionalIsoMappings,
    warn = TRUE
  )

  # drop NAs introduced by ignoreCountries
  x <- x[!is.na(getItems(x, dim = 1)), ]

  # manual corrections ----

  # BRG (West Germany before reunification) should be DEU after 1990
  # TODO: this could be a feature in toolISOhistorical, if we want to actively enhance the tool
  if ("BRG" %in% getItems(x, dim = 1) && any(getYears(x, as.integer = TRUE) > 1990)) {
    x <- add_columns(x, addnm = "DEU", dim = 1, fill = NA)
    x["DEU", getYears(x, as.integer = TRUE) > 1990, ] <- x["BRG", getYears(x, as.integer = TRUE) > 1990, ]
    x <- x["BRG", , invert = TRUE]
  }

  # split BLX into LUX and BEL

  if ("BLX" %in% getItems(x, dim = 1) && !any(c("LUX", "BEL") %in% getItems(x, dim = 1))) {

    x <- add_columns(x, addnm = c("BEL", "LUX"), dim = 1, fill = NA)
    x["BEL", , ] <- x["BLX", , ] * 0.8
    x["LUX", , ] <- x["BLX", , ] * 0.2
    x <- x["BLX", , , invert = TRUE]
  }
  return(x)
}
