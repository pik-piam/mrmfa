#' Calc Cullen Static Parameters
#' @description
#' Calc static (singular) steel parameters based on supplementary information of
#' Cullen et al. (2012) paper
#' 'Mapping the global flow of steel: from steelmaking to end‐use goods'. See
#' \link{readCullen2012} and the folder Cullen2012 in sources
#' for details on the preprocessing.
#' @param subtype Subtype of static parameter to calculate. Currently supported
#' subtypes are:
#' 'productionLossRate', 'formingLossRate', 'formingYield', 'fabricationYield'.
#' @author Merlin Jo Hosak
calcStCullenStaticParameters <- function(subtype) {
  # internal functions ----

  .calculateCullenFormingInflow <- function(flows) {
    # calculate total inflow ----
    bloomInflow <- dimSums(flows[, , "-> CC bloom", pmatch = TRUE], dim = 3)
    billetInflow <- dimSums(flows[, , "-> CC billet", pmatch = TRUE], dim = 3)
    slabInflow <- dimSums(flows[, , "-> CC slab", pmatch = TRUE], dim = 3)
    ingotInflow <- dimSums(flows[, , "-> Ingot casting", pmatch = TRUE], dim = 3)
    steelCastingInflow <- dimSums(flows[, , "-> Steel product casting", pmatch = TRUE], dim = 3)
    ironCastingInflow <- dimSums(flows[, , "-> Foundary iron casting", pmatch = TRUE], dim = 3)

    castingInflow <- bloomInflow + billetInflow + slabInflow + ingotInflow +
      steelCastingInflow + ironCastingInflow

    # calculate circular flows ----
    internalRecycling <- dimSums(flows[, , "Scrap (int recycle)", pmatch = TRUE], dim = 3)
    ingot2steelCasting <- flows[, , "Ingot Out -> Steel product casting", pmatch = TRUE]

    circularFlows <- internalRecycling + ingot2steelCasting

    # subtract circular flows from total inflow ----

    castingInflow <- castingInflow - circularFlows

    return(castingInflow)
  }

  .calcCullenFormingLoss <- function(lossFlows) {
    continousCastingLoss <- dimSums(lossFlows[, , "CC", pmatch = TRUE], dim = 3)
    otherCastingLoss <- dimSums(lossFlows[, , "casting", pmatch = TRUE], dim = 3)
    millLoss <- dimSums(lossFlows[, , "Mill", pmatch = TRUE], dim = 3)
    formingLoss <- continousCastingLoss + otherCastingLoss + millLoss
    return(formingLoss)
  }

  # read in source ----

  flows <- readSource("Cullen2012", subtype = "flows")
  lossFlows <- flows[, , "Loss", pmatch = TRUE]

  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "productionLossRate" = function() {
      # calculate production inflow ----

      bofInflow <- dimSums(flows[, , "-> BOF", pmatch = TRUE], dim = 3)
      eafInflow <- dimSums(flows[, , "-> EAF", pmatch = TRUE], dim = 3)
      ohfInflow <- dimSums(flows[, , "-> OHF", pmatch = TRUE], dim = 3)

      productionInflow <- bofInflow + eafInflow + ohfInflow

      # calculate production loss ----

      bofLoss <- lossFlows[, , "BOF", pmatch = TRUE]
      eafLoss <- lossFlows[, , "EAF", pmatch = TRUE]
      ohfLoss <- lossFlows[, , "OHF", pmatch = TRUE]
      secondaryMetallurgyLoss <- lossFlows[, , "Secondary metallurgy", pmatch = TRUE]
      productionLoss <- bofLoss + eafLoss + ohfLoss + secondaryMetallurgyLoss

      productionLossRate <- productionLoss / productionInflow

      return(productionLossRate)
    },
    "formingLossRate" = function() {
      formingInflow <- .calculateCullenFormingInflow(flows)
      formingLoss <- .calcCullenFormingLoss(lossFlows)
      formingLossRate <- formingLoss / formingInflow
      return(formingLossRate)
    },
    "formingYield" = function() {
      formingInflow <- .calculateCullenFormingInflow(flows)
      formingScrap <- dimSums(flows[, , "-> Scrap (Scrap)", pmatch = TRUE], dim = 3)
      formingLoss <- .calcCullenFormingLoss(lossFlows)
      formingYield <- (formingInflow - formingScrap - formingLoss) / formingInflow
      return(formingYield)
    },
    "fabricationYield" = function() {
      fabricationInflow <- dimSums(flows[, , "-> Finished", pmatch = TRUE], dim = 3)
      fabricationScrap <- flows[, , "Fabrication scrap", pmatch = TRUE]
      fabricationYield <- (fabricationInflow - fabricationScrap) / fabricationInflow
      return(fabricationYield)
    }
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(
      "Invalid subtype -- supported subtypes are:",
      paste0(names(switchboard), collapse = ", ")
    )
  } else {
    # ---- load data and do whatever ----
    data <- switchboard[[subtype]]()
    getItems(data, dim = 3) <- "value"
    getSets(data)["d3.1"] <- subtype

    final <- list(
      x = data,
      weight = NULL,
      description = subtype,
      unit = 1,
      isocountries = FALSE
    )
    return(final)
  }
}
