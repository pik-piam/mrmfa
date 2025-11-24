#' Convert World Steel Digitised
#' @description Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
convertWorldSteelDigitised <- function(x, subtype) {
  # TODO: make sure all the subtypes have a working convert function (so far, only "production)

  if (subtype == "indirectTrade") {
    x <- add_columns(x, addnm = c("BEL", "LUX", "SRB", "MNE"), dim = 1)

    # distribute Belgium Luxemburg 80/20 %
    x["BEL", ] <- x["BLX", , ] * 0.8
    x["LUX", ] <- x["BLX", , ] * 0.2
    x <- x["BLX", , , invert = TRUE]

    # distribute Serbia Montenegro 90/10 %
    x["SRB", ] <- x["SCG", , ] * 0.9
    x["MNE", ] <- x["SCG", , ] * 0.1
    x <- x["SCG", , , invert = TRUE]

    x <- toolCountryFill(x, verbosity = 2)

    return(x)
  } else {

    # add regions not present in the magpie object yet needed for toolISOhistorical to work
    countries <- getItems(x, dim = 1)

    mapping <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
      filter(.data$fromISO %in% countries)

    # use additional mapping for BLX
    blx <- data.frame(
      fromISO = "BLX",
      toISO = c("BEL", "LUX"),
      lastYear = "y2003"
    )

    # use additional mapping for SAC
    sac <- data.frame(
      fromISO = "SAC",
      toISO = c("BWA", "ZAF", "LSO", "SWZ", "NAM"),
      lastYear = "y2005"
    )

    newCountries <- c(unique(mapping$toISO), blx$toISO, sac$toISO)
    missingCountries <- setdiff(newCountries, countries)
    x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

    x <- toolISOhistorical(x, additional_mapping = rbind(blx, sac), overwrite = TRUE)

    # YUG in the 70s is split among others into SCG, not into MNE and SRB (as, the last year for SCG is 2005)
    # add SCG (former Serbia and Montenegro) to Serbia by hand and delete it
    if ("SCG" %in% getItems(x, dim = 1)) {
      x <- add_columns(x, addnm = "SRB", dim = 1, fill = NA)
      x["SRB", , ] <- x["SCG", , ]
      x <- x["SCG", , invert = TRUE]
    }

    x <- toolCountryFill(x, verbosity = 2)

    return(x)
  }
}
