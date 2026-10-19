#' Convert clinker data from Andrew 2019.
#' @author Bennet Weiss
#' @param x Magpie object
convertAndrew2019 <- function(x) {
  # clinker data for a lot of countries is missing, those will default to NA
  no_remove_warning <- c("KSV")

  x["SRB", , ] <- x["SRB", , ] + toolNAreplace(x["KSV", , ])$x

  x <- madrat::toolISOhistorical(x)
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  return(x)
}
