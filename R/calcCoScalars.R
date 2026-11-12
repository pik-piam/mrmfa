#' Collect scalar parameters
#'
#' @description
#' Stores named scalar values (e.g. the start and end years of the model time
#' periods) in a magpie object, so that they can be written to a file via
#' calcOutput. Written as .cs4r, each line has the form \code{name,value}.
#'
#' @author Jakob Dürrwächter
#' @param scalars Named numeric vector, e.g.
#' \code{c(start_historical = 1900, end_historical = 2022, end_future = 2150)}.
#' @param unit Unit of the scalars.
#' @return List with magpie object of the scalars and metadata in calcOutput format.
#' @examples
#' \dontrun{
#' calcOutput("CoScalars", scalars = c(start_historical = 1900, end_historical = 2022),
#'            file = "st_scalars.cs4r", aggregate = FALSE)
#' }
calcCoScalars <- function(scalars, unit = "various") {
  if (!is.numeric(scalars) || length(scalars) == 0) {
    stop("scalars must be a non-empty numeric vector.")
  }
  if (is.null(names(scalars)) || any(names(scalars) == "") || anyDuplicated(names(scalars))) {
    stop("All scalars must have unique, non-empty names.")
  }

  x <- new.magpie(names = names(scalars), fill = unname(scalars), sets = c("region", "year", "scalar"))

  return(list(
    x = x,
    weight = NULL,
    unit = unit,
    isocountries = FALSE,
    description = paste0("Scalar parameters: ", paste(names(scalars), collapse = ", ")),
    note = "dimensions: (scalar,value)"
  ))
}
