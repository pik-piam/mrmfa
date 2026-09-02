#' Dummy function to create magpie object with rates that are historically 0.
#' E.g. for chemical recycling, bio-based production and DAC based production
#'
#' @author Leonie Schweiger
#'
calcPlZeroRates <- function() {

  x <- new.magpie(fill = 0)

  return(list(
    x           = x,
    weight      = NULL,
    unit        = "share",
    isocountries = FALSE,
    description = glue::glue("Rates that are historically 0, e.g. chemical recycling \\
                             rate, bio-based & DAC production rate and emission capture rate"),
    note        = "dimensions: (value)"
  ))
}
