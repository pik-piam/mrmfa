#' Load Cullen 2012 fabrication yield
#' @description 
#' Function to load fabrication yield data from Cullen et al. (2012). 
#' Using the function \link[readCullen2012]{readCullen2012}.
#' @author Merlin Jo Hosak
#' @export
calcCullenFabricationYield <- function() {
  # Read data
  giMatrix <- readSource('Cullen2012', subtype='giMatrix', convert=F)
  
  # Extract relevant cells from matrix and bind do one magpie
  
  parameters <- getItems(giMatrix, dim = 3)
  endUseGoods <- giMatrix[,,grep('End Use Goods - End Use Goods', parameters, fixed = TRUE)]
  endUseGoodsParameters <- getItems(endUseGoods, dim = 3)
  
  patterns <- c("Construction %", "Machinery %", "Products %", "Transport %")
  final <- do.call(
    mbind,
    lapply(patterns, function(p) endUseGoods[,,grep(p, endUseGoodsParameters, fixed = TRUE)])
  )
  
  # Finalize
  
  getSets(final)<-c('region','year','parameter')
  getItems(final, dim=3) <- c('Construction Yield',
                              'Machinery Yield',
                              'Products Yield',
                              'Transport Yield')
  
  final <- list(x = final, 
                weight = NULL,
                unit=1,
                description='Cullen 2012 Fabrication Yield')
  
  return(final)
}