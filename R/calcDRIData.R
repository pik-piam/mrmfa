#' Calc DRI data
#' 
#' @description
#' Cald direct reduced iron data mostly based on World Steel Association
#' data (see |link[readWorldSteelDatabase]{readWorldSteelDatabase} for details).
#' The DRI data is backcasted based on steel production data to fill in missing
#' years.
#' 
#' @param subtype Options: 'production', 'imports', 'exports'
#' 
#' @author Merlin Jo Hosak
#' @export
calcDRIData <- function(subtype) {
  driSubtype <- paste('dri', tools::toTitleCase(subtype), sep='')
  driData <- readSource('WorldSteelDatabase', subtype=driSubtype)
  steelProduction <- calcOutput('SteelProduction', aggregate=F)
  
  # Backcast DRI production based on steel production
  final <- toolBackcastByReference2D(driData, steelProduction)
  result <- list(x = final, 
                 weight = NULL,
                 unit='Tonnes',
                 description=paste('DRI data of type', subtype,'from 1900-2022 yearly for the SIMSON format'))
  
  return(result)
}