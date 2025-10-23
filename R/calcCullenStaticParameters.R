#' @author Merlin Jo Hosak
#' @export
calcCullenStaticParameters <- function() {
  flows <- readSource('Cullen2012', subtype='flows')
  flowNames <- getItems(flows, dim = 3)
  lossFlows <- flows[,,grep('Loss', flowNames, fixed = TRUE)]
  lossFlowNames <- getItems(lossFlows, dim = 3)
  
  final <- new.magpie(
    names = c('Production Loss',
              'Forming Loss',
              'Forming Yield',
              'Fabrication Yield'),
  )
  
  productionInflow <- calcCullenProductionInflow(flows, flowNames)
  productionLoss <- calcCullenProductionLoss(flows, lossFlows, lossFlowNames)
  
  formingInflow <- calcCullenFormingInflow(flows, flowNames)
  formingScrap <- calcCullenFormingScrap(flows, flowNames)
  formingLoss <- calcCullenFormingLoss(flows, lossFlows, lossFlowNames)
  
  fabricationInflow <- dimSums(flows[,,grep('-> Finished', flowNames, fixed = TRUE)], dim = 3)
  fabricationScrap <- flows[,,grep('Fabrication scrap', flowNames, fixed = TRUE)]
  
  final[,, 'Production Loss'] <- (productionLoss / productionInflow)[,,1]
  final[,, 'Forming Loss'] <- (formingLoss/formingInflow)[,,1]
  final[,, 'Forming Yield'] <- ((formingInflow - formingScrap - formingLoss) / formingInflow)[,,1]
  final[,, 'Fabrication Yield'] <- ((fabricationInflow - fabricationScrap) 
                                    / fabricationInflow)[,,1]
  
  final <- list(x = final, 
                weight = NULL,
                unit=1,
                description='Cullen 2012 static parameters')
  
  return(final)
}

calcCullenProductionInflow <- function(flows, flowNames = getItems(flows, dim = 3)) {
  bofInflow <- dimSums(flows[,,grep('-> BOF', flowNames, fixed = TRUE)], dim = 3)
  eafInflow <- dimSums(flows[,,grep('-> EAF', flowNames, fixed = TRUE)], dim = 3)
  ohfInflow <- dimSums(flows[,,grep('-> OHF', flowNames, fixed = TRUE)], dim = 3)
  
  productionInflow <- bofInflow + eafInflow + ohfInflow 
  
  return(productionInflow)
}

calcCullenProductionLoss <- function(flows,
                                     lossFlows=flows[,,grep('Loss', getItems(flows,dim=3), fixed = TRUE)],
                                     lossFlowNames=getItems(lossFlows,dim=3)) {
  bofLoss <- lossFlows[,,grep('BOF', lossFlowNames, fixed = TRUE)]
  eafLoss <- lossFlows[,,grep('EAF', lossFlowNames, fixed = TRUE)]
  ohfLoss <- lossFlows[,,grep('OHF', lossFlowNames, fixed = TRUE)]
  secondaryMetallurgyLoss <- lossFlows[,,grep('Secondary metallurgy', lossFlowNames, fixed = TRUE)]
  
  productionLoss <- bofLoss + eafLoss + ohfLoss + secondaryMetallurgyLoss
  
  return(productionLoss)
}


calcCullenFormingInflow <- function(flows, flowNames = getItems(flows, dim = 3)) {
  bloomInflow <- dimSums(flows[,,grep('-> CC bloom', flowNames, fixed = TRUE)], dim = 3)
  billetInflow <- dimSums(flows[,,grep('-> CC billet', flowNames, fixed = TRUE)], dim = 3)
  slabInflow <- dimSums(flows[,,grep('-> CC slab', flowNames, fixed = TRUE)], dim = 3)
  ingotInflow <- dimSums(flows[,,grep('-> Ingot casting', flowNames, fixed = TRUE)], dim = 3)
  steelCastingInflow <- dimSums(flows[,,grep('-> Steel product casting', flowNames, fixed = TRUE)], dim = 3)
  ironCastingInflow <- dimSums(flows[,,grep('-> Foundary iron casting', flowNames, fixed = TRUE)], dim = 3)
  
  castingInflow <- bloomInflow + billetInflow + slabInflow + ingotInflow + steelCastingInflow + ironCastingInflow
  
  # avoid circular flows
  internalRecycling <- dimSums(flows[,,grep('Scrap (int. recycle)', flowNames, fixed = TRUE)], dim = 3)
  ingot2steelCasting <- flows[,,grep('Ingot Out -> Steel product casting', flowNames, fixed = TRUE)]
  
  castingInflow <- castingInflow - internalRecycling - ingot2steelCasting
  
  return(castingInflow)
}

calcCullenFormingScrap <- function(flows, flowNames = getItems(flows, dim = 3)) {
  formingScrap <- flows[,,grep('-> Scrap (Scrap)', flowNames, fixed = TRUE)]
  formingScrap <- dimSums(formingScrap, dim = 3)
  
  return(formingScrap)
}

calcCullenFormingLoss <- function(flows,
                                     lossFlows=flows[,,grep('Loss', getItems(flows,dim=3), fixed = TRUE)],
                                     lossFlowNames=getItems(lossFlows,dim=3)) {
  continousCastingLoss <- dimSums(lossFlows[,,grep('CC', lossFlowNames, fixed = TRUE)], dim=3)
  otherCastingLoss <- dimSums(lossFlows[,,grep('casting', lossFlowNames, fixed = TRUE)],dim=3)
  millLoss <- dimSums(lossFlows[,,grep('Mill', lossFlowNames, fixed = TRUE)], dim=3)
  
  formingLoss <- continousCastingLoss + otherCastingLoss + millLoss
  
  return(formingLoss)
}










