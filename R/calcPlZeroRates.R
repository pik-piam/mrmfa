#' Dummy function to create magpie object with rates that are historically 0.
#' E.g. for chemical recycling, bio-based production and DAC based production
#'
#' @author Leonie Schweiger
#'
calcPlZeroRates <- function() {

  x <- new.magpie(
    cells_and_regions = madrat::getISOlist(),
    years = 1950:2019,
    names = NULL,
    fill = 0,
  )

  weight <- x
  weight[, ] <- 1

  return(list(
    x           = x,
    weight      = weight,
    unit        = "share",
    description = "Rates that are historically 0, e.g. chemical recycling rate, bio-based & DAC production rate and emission capture rate",
    note        = "dimensions: (Historic Time,Region,value)"
  ))
}
