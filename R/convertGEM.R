#' Convert buildings split data from GEM.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertGEM <- function(x) {
  no_remove_warning <- "XKX" # Kosovo treated as part of Serbia, Serbia already exists.
  x_out <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  return(x_out)
}
