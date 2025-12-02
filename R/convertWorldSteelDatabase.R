#' Convert World Steel Database
#' @description Convert data from WorldSteelAssociation Database
#' @author Merlin Jo Hosak
#' @param x Magpie object
#' @inherit readWorldSteelDatabase
convertWorldSteelDatabase <- function(x, subtype = "production") {

  # TODO: for some subtypes, we split BLX and SCG according to fixed rules, for others we use
  # toolISOhistorical. Should this be unified?

  # Special case for splitting BLX and SCG ----

  if (subtype %in% c("indirectImports", "indirectExports")) {

    x <- add_columns(x, addnm = c("BEL", "LUX", "SRB", "MNE"), dim = 1)

    # distribute Belgium Luxemburg 80/20 %
    x["BEL", ] <- x["BLX", ] * 0.8
    x["LUX", ] <- x["BLX", ] * 0.2
    x <- x["BLX", , , invert = TRUE]

    # distribute Serbia Montenegro 90/10 %
    x["SRB", ] <- x["SCG", ] * 0.9
    x["MNE", ] <- x["SCG", ] * 0.1
    x <- x["SCG", , , invert = TRUE]

  }

  # TODO make subtypes explicit?
  if (any(c("BLX", "YUG", "SCG") %in% getRegions(x))) {

    # Add historical mapping for Yugoslavia with last year being 2005
    # instead of 1991 as there is some aggregated data in this dataset
    # for the years 2002-2005 for Yugoslavia. Similar for Belgium and Luxembourg.

    yugoslavia <- data.frame(
      fromISO = "YUG",
      toISO = c("SRB", "MNE", "SVN", "HRV", "MKD", "BIH"),
      lastYear = "y2005"
    )

    blx <- data.frame(
      fromISO = "BLX",
      toISO = c("BEL", "LUX"),
      lastYear = "y2003"
    )

    scg <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
      filter(.data$fromISO == "SCG")

    historicalMapping <- rbind(yugoslavia, blx, scg) %>%
      filter(.data$fromISO %in% getItems(x, dim = 1))

    # add countries missing yet that will get data after YUG/BLX split
    newCountries <- historicalMapping$toISO
    missingCountries <- setdiff(newCountries, getItems(x, dim = 1))
    x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

    x <- toolISOhistorical(x, mapping = historicalMapping, overwrite = TRUE)
  }

  x <- toolCountryFill(x, verbosity = 2)

  return(x)
}
