#' Calc pig iron trade
#'
#' @description
#' Calculate pig iron trade Wrapper function. See
#' \link{calcStPigIronPreliminaryData} for specifics.
#'
#' @param subtype 'imports' or 'exports' to calculate pig iron imports or
#' exports respectively.
#'
#' @author Merlin Jo Hosak
#'
calcStPigIronTrade <- function(subtype) {
  tradeData <- calcOutput("StPigIronPreliminaryData", subtype = subtype, aggregate = FALSE)
  production <- calcOutput("StPigIronPreliminaryData", subtype = "production", aggregate = FALSE)

  final <- toolBackcastByReference2D(tradeData, production)

  result <- list(
    x = final,
    weight = NULL,
    unit = "Tonnes",
    description = paste("Pig iron data of type", subtype, "from 1900-2022 yearly for the SIMSON format")
  )

  return(result)
}
