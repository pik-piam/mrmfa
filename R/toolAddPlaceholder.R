#' Append placeholder categories to a magpie object.
#' Useful to extend data with "N/A" entries for categories that are not covered by the source data.
#' @author Bennet Weiss
#' @param x Magpie object
#' @param names Names of the new entries in dim 3. Subdimensions are separated by dots, e.g. "Ind.N/A".
#' @param fill Value the new entries are filled with.
toolAddPlaceholder <- function(x, names, fill = 1) {
  # idempotent: only add entries that do not exist yet
  names <- setdiff(names, getItems(x, dim = 3))
  if (length(names) == 0) {
    return(x)
  }
  placeholder <- new.magpie(
    cells_and_regions = getItems(x, dim = 1),
    years = getYears(x),
    names = names,
    fill = fill,
    sets = getSets(x, fulldim = FALSE)
  )
  out <- mbind(x, placeholder)
  return(out)
}
