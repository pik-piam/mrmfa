#' Calculate the share of cement in a cement product.
#'
#' @author Bennet Weiss
#'
calcCeCementRatio <- function() {

  # TODO read-in this data from ODYM-RECC
  # 13% cement in concrete and mortar
  # used in ODYM RECC 2.4
  cement_ratio <- 0.13
  x <- new.magpie(fill = cement_ratio)

  unit <- "ratio"
  description <- "Share of cement in a cement product by weight."
  note <- "dimensions: (value)"

  output <- list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    note = note,
    isocountries = FALSE
  )
  return(output)
}
