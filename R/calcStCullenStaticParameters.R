#' Calc Cullen Static Parameters
#' @description
#' Calc static (singular) steel parameters based on supplemnetar information of
#' Cullen et al. (2012) paper
#' 'Mapping the global flow of steel: from steelmaking to end‐use goods'. See
#' \link{readCullen2012} and the folder Cullen2012 in sources
#' for details on the preprocessing.
#' @param subtype Subtype of static parameter to calculate. Currently supported
#' subtypes are:
#' 'productionLossRate', 'formingLossRate', 'formingYield', 'fabricationYield'.
#' @author Merlin Jo Hosak
#' @export
calcStCullenStaticParameters <- function(subtype) {
  flows <- readSource("Cullen2012", subtype = "flows")
  flowNames <- getItems(flows, dim = 3)
  lossFlows <- flows[, , grep("Loss", flowNames, fixed = TRUE)]
  lossFlowNames <- getItems(lossFlows, dim = 3)

  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "productionLossRate" = function() {
      productionInflow <- calcCullenProductionInflow(flows, flowNames)
      productionLoss <- calcCullenProductionLoss(flows, lossFlows, lossFlowNames)

      productionLossRate <- productionLoss / productionInflow

      return(productionLossRate)
    },
    "formingLossRate" = function() {
      formingInflow <- calcCullenFormingInflow(flows, flowNames)
      formingScrap <- calcCullenFormingScrap(flows, flowNames)
      formingLoss <- calcCullenFormingLoss(flows, lossFlows, lossFlowNames)

      fabricationInflow <- dimSums(flows[, , grep("-> Finished", flowNames, fixed = TRUE)], dim = 3)
      fabricationScrap <- flows[, , grep("Fabrication scrap", flowNames, fixed = TRUE)]

      formingLossRate <- formingLoss / formingInflow

      return(formingLossRate)
    },
    "formingYield" = function() {
      formingInflow <- calcCullenFormingInflow(flows, flowNames)
      formingScrap <- calcCullenFormingScrap(flows, flowNames)
      formingLoss <- calcCullenFormingLoss(flows, lossFlows, lossFlowNames)

      formingYield <- (formingInflow - formingScrap - formingLoss) / formingInflow

      return(formingYield)
    },
    "fabricationYield" = function() {
      fabricationInflow <- dimSums(flows[, , grep("-> Finished", flowNames, fixed = TRUE)], dim = 3)
      fabricationScrap <- flows[, , grep("Fabrication scrap", flowNames, fixed = TRUE)]

      fabricationYield <- (fabricationInflow - fabricationScrap) / fabricationInflow

      return(fabricationYield)
    },
    NULL
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    data <- switchboard[[subtype]]()
    getItems(data, dim = 3) <- "value"
    getSets(data)["d3.1"] <- subtype

    final <- list(
      x = data,
      weights = NULL,
      description = subtype,
      unit = 1
    )
    return(final)
  }
}

calcCullenProductionInflow <- function(flows, flowNames = getItems(flows, dim = 3)) {
  bofInflow <- dimSums(flows[, , grep("-> BOF", flowNames, fixed = TRUE)], dim = 3)
  eafInflow <- dimSums(flows[, , grep("-> EAF", flowNames, fixed = TRUE)], dim = 3)
  ohfInflow <- dimSums(flows[, , grep("-> OHF", flowNames, fixed = TRUE)], dim = 3)

  productionInflow <- bofInflow + eafInflow + ohfInflow

  return(productionInflow)
}

calcCullenProductionLoss <- function(flows, lossFlows, lossFlowNames) {
  bofLoss <- lossFlows[, , grep("BOF", lossFlowNames, fixed = TRUE)]
  eafLoss <- lossFlows[, , grep("EAF", lossFlowNames, fixed = TRUE)]
  ohfLoss <- lossFlows[, , grep("OHF", lossFlowNames, fixed = TRUE)]
  secondaryMetallurgyLoss <- lossFlows[, , grep("Secondary metallurgy", lossFlowNames, fixed = TRUE)]

  productionLoss <- bofLoss + eafLoss + ohfLoss + secondaryMetallurgyLoss

  return(productionLoss)
}


calcCullenFormingInflow <- function(flows, flowNames = getItems(flows, dim = 3)) {
  # Calculate total inflow

  bloomInflow <- dimSums(flows[, , grep("-> CC bloom", flowNames, fixed = TRUE)], dim = 3)
  billetInflow <- dimSums(flows[, , grep("-> CC billet", flowNames, fixed = TRUE)], dim = 3)
  slabInflow <- dimSums(flows[, , grep("-> CC slab", flowNames, fixed = TRUE)], dim = 3)
  ingotInflow <- dimSums(flows[, , grep("-> Ingot casting", flowNames, fixed = TRUE)], dim = 3)
  steelCastingInflow <- dimSums(flows[, , grep("-> Steel product casting", flowNames, fixed = TRUE)], dim = 3)
  ironCastingInflow <- dimSums(flows[, , grep("-> Foundary iron casting", flowNames, fixed = TRUE)], dim = 3)

  castingInflow <- bloomInflow + billetInflow + slabInflow + ingotInflow + steelCastingInflow + ironCastingInflow

  # calculate circular flows

  internalRecycling <- dimSums(flows[, , grep("Scrap (int. recycle)", flowNames, fixed = TRUE)], dim = 3)
  ingot2steelCasting <- flows[, , grep("Ingot Out -> Steel product casting", flowNames, fixed = TRUE)]

  circularFlows <- internalRecycling + ingot2steelCasting

  # Subtract circular flows from total inflow

  castingInflow <- castingInflow - circularFlows

  return(castingInflow)
}

calcCullenFormingScrap <- function(flows, flowNames = getItems(flows, dim = 3)) {
  formingScrap <- flows[, , grep("-> Scrap (Scrap)", flowNames, fixed = TRUE)]
  formingScrap <- dimSums(formingScrap, dim = 3)

  return(formingScrap)
}

calcCullenFormingLoss <- function(flows, lossFlows, lossFlowNames) {
  continousCastingLoss <- dimSums(lossFlows[, , grep("CC", lossFlowNames, fixed = TRUE)], dim = 3)
  otherCastingLoss <- dimSums(lossFlows[, , grep("casting", lossFlowNames, fixed = TRUE)], dim = 3)
  millLoss <- dimSums(lossFlows[, , grep("Mill", lossFlowNames, fixed = TRUE)], dim = 3)

  formingLoss <- continousCastingLoss + otherCastingLoss + millLoss

  return(formingLoss)
}
