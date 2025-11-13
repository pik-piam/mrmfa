#' Convert World Steel Database
#' @description Convert data from WorldSteelAssociation Database
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDatabase <- function(x, subtype = "production") {

  # Special case for splitting BLX ----

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

  # Special case for splitting YUG ----

  # distribute YUG 2003-2005 to SRB and MNE only, as the other YUG countries are already
  # reported separately


  if (subtype %in% c(
    "production", "eafProduction", "imports", "exports",
    "scrapImports", "scrapExports", "pigIronImports", "pigIronExports",
    "driImports", "driExports"
  )) {
    yugoslavia <- data.frame(
      fromISO = "YUG",
      toISO = c("SRB", "MNE"),
      lastYear = "y2005"
    )

    x <- toolISOhistorical(x, overwrite = TRUE, mapping = yugoslavia)
  }


  # General case for splitting of BLX and YUG ----

  if (any(c("BLX", "YUG") %in% getRegions(x))) {
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

    historicalMapping <- rbind(yugoslavia, blx)

    # add countries missing yet that will get data after YUG/BLX split
    newCountries <- historicalMapping$toISO
    missingCountries <- setdiff(newCountries, getItems(x, dim = 1))
    x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

    x <- toolISOhistorical(x, overwrite = TRUE, mapping = historicalMapping)
  }

  x <- x[!is.na(getItems(x, dim = 1)), ] # remove rows with NA in country_name column
  x <- toolCountryFill(x, verbosity = 2)

  return(x)
}
