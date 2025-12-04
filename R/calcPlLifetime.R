#' Calculates the mean lifetimes of plastic goods by use sector, or their standard deviations.
#' @author Leonie Schweiger
#' @param subtype Character string specifying to read means or standard deviations from data
#'        - "Lifetime_mean"
#'        - "Lifetime_std"
calcPlLifetime <- function(subtype) {
  data_raw <- readSource("Geyer", subtype = subtype, convert = FALSE)
  sector_map <- toolGetMapping(
    "structuremappingPlasticGeyerLifetime.csv",
    type = "sectoral", where = "mrmfa"
  )
  data <- toolAggregate(
    data_raw,
    rel = sector_map, dim = 3,
    from = "Source", to = "Target"
  )
  # remove category "Industrial Machinery"
  data <- data[, , getItems(data, 3) != "Industrial Machinery"]
  description <- paste(
    subtype,
    " of plastic goods by use sector. ",
    "Data from Geyer et al. 2017 https://doi.org/10.1126/sciadv.1700782"
  )
  output <- list(
    x = data,
    weight = NULL,
    unit = "years (a)",
    description = description,
    isocountries = FALSE,
    note = "dimensions: (Good,value)"
  )
  return(output)
}
