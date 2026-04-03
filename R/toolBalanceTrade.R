#' Balance trade flows by scaling imports and exports to a common reference
#'
#' @description
#' Scales country-level imports and exports so that global totals match a
#' chosen reference value. The default harmonic-mean method is the same
#' approach used by Pehl et al. for trade balancing.
#'
#' @param imports magpie object with import flows by region.
#' @param exports magpie object with export flows by region.
#' @param to character. Reference to balance to. One of \code{"hmean"} (default),
#'   \code{"gmean"}, \code{"amean"}, \code{"maximum"}, \code{"minimum"},
#'   \code{"imports"}, or \code{"exports"}.
#'
#' @return list with elements \code{imports} and \code{exports} containing the
#'   balanced magpie objects.
#'
#' @author Bennet Weiss
toolBalanceTrade <- function(imports, exports, to = "hmean") {
  global_imports <- dimSums(imports, dim = 1)
  global_exports <- dimSums(exports, dim = 1)

  reference_trade <- switch(to,
    maximum = {
      result <- global_imports
      mask   <- global_exports > global_imports
      result[mask] <- global_exports[mask]
      result
    },
    minimum = {
      result <- global_imports
      mask   <- global_exports < global_imports
      result[mask] <- global_exports[mask]
      result
    },
    imports = global_imports,
    exports = global_exports,
    hmean = {
      denom        <- global_imports + global_exports
      denom[denom == 0] <- NA
      2 * global_imports * global_exports / denom
    },
    gmean = sqrt(global_imports * global_exports),
    amean = (global_imports + global_exports) / 2,
    stop(paste0(
      "Can't balance to '", to, "', method not recognized. Must be one of ",
      "'maximum', 'minimum', 'imports', 'exports', 'hmean', 'gmean', 'amean'."
    ))
  )

  reference_trade[is.na(reference_trade)] <- 0

  # protect against division by zero when global total is zero
  

  eps <- .Machine$double.eps
  global_imports_safe <- global_imports
  global_imports_safe[global_imports_safe < eps] <- eps
  global_exports_safe <- global_exports
  global_exports_safe[global_exports_safe < eps] <- eps

  import_factor <- reference_trade / global_imports_safe
  export_factor <- reference_trade / global_exports_safe

  list(
    imports = imports * import_factor,
    exports = exports * export_factor
  )
}
