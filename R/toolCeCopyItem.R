#' Copy a single data item to new item names.
#' Used to state explicitly which values are transferred to items without own data.
#'
#' @author Bennet Weiss
#' @param x magpie object with a single item in the data dimension.
#' @param items names of the new items.
#' @param dim name of the new data dimension.
toolCeCopyItem <- function(x, items, dim) {
  if (ndata(x) != 1) {
    stop("x must contain exactly one data item.")
  }
  out <- x[, , rep(1, length(items))]
  getItems(out, dim = 3, raw = TRUE) <- items
  getSets(out, fulldim = FALSE)[3] <- dim
  return(out)
}
