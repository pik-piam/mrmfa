#' Expert guess on the relative standard deviation of lifetime
#' of buildings and infrastructure
#'
#' @author Bennet Weiss
calcCeLifetimeRelStd <- function() {
  rel_lifetime <- 0.4
  x <- as.magpie(rel_lifetime)

  unit <- "ratio"
  description <- paste(
    "Relative standard deviation of the use lifetime of buildings and ",
    "infrastructure. Based on expert judgement."
  )
  note <- "dimensions: (value)"
  output <- list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    note = note
  )
}
