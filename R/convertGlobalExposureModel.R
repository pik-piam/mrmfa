#' Convert buildings split data from GEM.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertGlobalExposureModel <- function(x) {
  x <- replace_non_finite(x, replace = 0)

  structureMapping <- toolGetMapping("CeBuildingStructureMapping.csv", type = "sectoral", where = "mrmfa")
  x <- toolAggregate(
    x,
    rel = structureMapping,
    dim = 3.2,
    from = "GEM_structure",
    to = "RASMI_structure",
  )

  x["SRB", ] <- x["SRB", ] + toolNAreplace(x["XKX", ])$x # add Kosovo to Serbia
  x_out <- madrat::toolCountryFill(x, verbosity = 2, no_remove_warning = "XKX")
  return(x_out)
}
