#' Convert World Steel Digitised
#' @description Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @inherit readWorldSteelDigitised
#' @param x MagPIE object
#' @author Merlin Jo Hosak
convertWorldSteelDigitised <- function(x, subtype) {

  # TODO complete
  if (subtype %in% c("worldProduction", "historicScrapShare",
                     "scrapConsumption", "worldScrapConsumption")) {
    stop("convert not supported for subtype '", subtype, "'")
  }

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
    # TODO make subtypes explicit
    # add regions not present in the magpie object yet needed for toolISOhistorical to work

    historicalMapping <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
      filter(.data$fromISO %in% getItems(x, dim = 1))

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

    scg <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
      filter(.data$fromISO == "SCG")

    newCountries <- c(unique(historicalMapping$toISO), blx$toISO, sac$toISO, scg$toISO)
    missingCountries <- setdiff(newCountries, getItems(x, dim = 1))
    x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)
    x <- toolISOhistorical(x, additional_mapping = rbind(blx, sac), overwrite = TRUE)

    # YUG in the 70s is split (among others) into SCG, not into MNE and SRB
    # (as, the last year in the data for SCG is 2005)
    # add SCG (former Serbia and Montenegro) to Serbia by hand and delete it
    if (subtype == "productionByProcess") {
      x["SRB", , ] <- x["SCG", , ]
      x <- x["SCG", , invert = TRUE]
    }

    x <- toolCountryFill(x, verbosity = 2)

    return(x)
  }
}
