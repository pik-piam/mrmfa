#' Calc pig iron trade
#'
#' @description
#' Calculate pig iron trade Wrapper function. See
#' \link[calcPigIronPreliminaryData]{calcPigIronPreliminaryData} for specifics.
#'
#' @param subtype 'imports' or 'exports' to calculate pig iron imports or
#' exports respectively.
#'
#' @author Merlin Jo Hosak
#'
#' @export
calcPigIronTrade <- function(subtype) {
  tradeData <- calcOutput("PigIronPreliminaryData", subtype = subtype, aggregate = F)
  production <- calcOutput("PigIronPreliminaryData", subtype = "production", aggregate = F)

  final <- toolBackcastByReference2D(tradeData, production)

  result <- list(
    x = final,
    weight = NULL,
    unit = "Tonnes",
    description = paste("Pig iron data of type", subtype, "from 1900-2022 yearly for the SIMSON format")
  )

  return(result)
}
