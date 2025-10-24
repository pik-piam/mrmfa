#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @export
calcStCullenFabricationYield <- function() {
  gi_matrix <- readSource('Cullen2012', subtype='gi_matrix', convert=F)
  
  parameters <- getItems(gi_matrix, dim = 3)
  endUseGoods <- gi_matrix[,,grep('End Use Goods - End Use Goods', parameters, fixed = TRUE)]
  endUseGoodsParameters <- getItems(endUseGoods, dim = 3)
  
  constructionYield <- endUseGoods[,,grep('Construction %', endUseGoodsParameters, fixed = TRUE)]
  machineryYield <- endUseGoods[,,grep('Machinery %', endUseGoodsParameters, fixed = TRUE)]
  productsYield <- endUseGoods[,,grep('Products %', endUseGoodsParameters, fixed = TRUE)]
  transportYield <- endUseGoods[,,grep('Transport %', endUseGoodsParameters, fixed = TRUE)]
  
  final <- mbind(constructionYield, machineryYield, productsYield, transportYield)
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