#' Calc DRI data
#'
#' @description
#' Calc direct reduced iron data mostly based on World Steel Association
#' data (see \link{readWorldSteelDatabase} for details).
#' The DRI data is backcasted based on steel production data to fill in missing
#' years.
#'
#' @param subtype Options: 'production', 'imports', 'exports'
#'
#' @author Merlin Jo Hosak
calcStDRIData <- function(subtype) {

  if (subtype == "production") {
    driData <- readSource("WorldSteelDatabase", subtype = "driProduction")
  } else if (subtype == "imports") {
    driData <- readSource("WorldSteelDatabase", subtype = "driImports")
  } else if (subtype == "exports") {
    driData <- readSource("WorldSteelDatabase", subtype = "driExports")
  }

  steelProduction <- calcOutput("StProduction", aggregate = FALSE)

  # Backcast DRI production based on steel production
  final <- toolBackcastByReference2D(driData, steelProduction)

  result <- list(
    x = final,
    weight = NULL,
    unit = "Tonnes",
    description = paste0("DRI data of type ", subtype, " from 1900-2022 yearly for the SIMSON format")
  )

  return(result)
}
