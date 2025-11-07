#' Calc pig iron production
#'
#' @description
#' Calculate pig iron production. Wrapper function. See
#' \link{calcStPigIronPreliminaryData} for specifics.
#'
#' @author Merlin Jo Hosak
#'
#' @export
calcStPigIronProduction <- function() {
  data <- calcOutput("StPigIronPreliminaryData", subtype = "production", aggregate = FALSE)

  # Pig Iron production Data doesn't need any more processing, wrapper function just there for completeness.

  result <- list(
    x = data,
    weight = NULL,
    unit = "Tonnes",
    description = "Pig iron production from 1900-2022 yearly for the SIMSON format"
  )

  return(result)
}
