#' Copy a single data item to new item names.
#'
#' Used to state explicitly which values are transferred to items without own data.
#' The data dimension of \code{x} is replaced: it is renamed to \code{newDim} and
#' contains one copy of the single data item of \code{x} per entry of \code{newItems}.
#'
#' @author Bennet Weiss
#' @param x magpie object with exactly one item in the data dimension.
#' @param newItems names of the new items.
#' @param newDim name of the new data dimension.
#' @return magpie object with the data dimension \code{newDim}, holding the items \code{newItems}.
#' @examples
#' \dontrun{
#' # use the value of C15 concrete for all mortar applications
#' toolCopyItem(x = concrete[, , "C15"], newItems = c("finishing", "masonry"), newDim = "Product Application")
#' }
toolCopyItem <- function(x, newItems, newDim) {
  if (ndata(x) != 1) {
    stop("x must contain exactly one data item, but contains ", ndata(x), ".")
  }
  # repeat the single data item once per new item
  out <- x[, , rep(1, length(newItems))]
  # replace item names and dimension name
  getItems(out, dim = 3, raw = TRUE) <- newItems
  getSets(out, fulldim = FALSE)[3] <- newDim
  return(out)
}
