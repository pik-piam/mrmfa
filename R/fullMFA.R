#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the
#' MFA model.
#'
#' @author Merlin HOSAK
#' @author Bennet Weiss
#' @param rev Revision number for the data version
#' @param dev Development version string
#' @param scenario SSP scenario used for population and GDP drivers
#' @param gdpPerCapita bool if GDP driver should be returned as per capita values
#' @param runSections Character vector selecting which parts to run.
#' Allowed values (see validSections): c("drivers", "steel", "cement", "plastic"). NULL (default) runs all.
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}},
#' \code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{
#' retrieveData("MFA")
#' fullMFA()
#' }
#'
fullMFA <- function(rev = 0, dev = "", scenario = "SSP2", gdpPerCapita = FALSE, runSections = NULL) {
  # prepare section selector
  validSections <- c("drivers", "steel", "cement", "plastic")

  if (is.null(runSections)) {
    runSections <- validSections
  } else {
    bad <- setdiff(runSections, validSections)
    if (length(bad)) stop("Invalid sections: ", paste(bad, collapse = ", "))
  }

  runSection <- function(name) name %in% runSections

  if (!length(runSections)) {
    message("fullMFA: no sections selected; nothing done.")
    return(invisible(NULL))
  }

  #  ------------- DRIVERS -------------
  if (runSection("drivers")) {
    calcOutput("CoPopulation1900To2150", file = "co_population1900To2150.cs4r", scenario = scenario)
    calcOutput("CoGDP1900To2150", file = "co_gdp1900To2150.cs4r", scenario = scenario, perCapita = gdpPerCapita)
  }

  #  ------------- STEEL ----------------
  if (runSection("steel")) {
    # Production
    calcOutput("StProduction", file = "st_steel_production.cs4r")
    calcOutput("StProductionByProcess", file = "st_steel_production_by_process.cs4r")
    # Trade
    calcOutput("StTrade", file = "st_steel_imports.cs4r", subtype = "imports")
    calcOutput("StTrade", file = "st_steel_exports.cs4r", subtype = "exports")
    calcOutput("StTrade", file = "st_steel_scrap_imports.cs4r", subtype = "scrapImports")
    calcOutput("StTrade", file = "st_steel_scrap_exports.cs4r", subtype = "scrapExports")
    calcOutput("StTrade", file = "st_steel_indirect_imports.cs4r", subtype = "indirectImports")
    calcOutput("StTrade", file = "st_steel_indirect_exports.cs4r", subtype = "indirectExports")
    # Parameters
    calcOutput("StCullenFabricationYield", file = "st_fabrication_yield.cs4r", aggregate = FALSE)
    calcOutput("StLifetimes", subtype = "Cooper2014", file = "st_lifetimes.cs4r", aggregate = FALSE)
    calcOutput("StRecoveryRate", subtype = "WorldSteel", file = "st_recovery_rate.cs4r", aggregate = FALSE)
    calcOutput("StSectorSplits", subtype = "Pauliuk2013", file = "st_sector_splits.cs4r", aggregate = FALSE)
    # Static Parameters
    calcOutput("StMaxScrapShare", subtype = "BIR", file = "st_max_scrap_share.cs4r", aggregate = FALSE)
    calcOutput("StWorldSteelStaticParameters", subtype = "scrapInBOFrate", file = "st_scrap_in_BOF_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "productionLossRate", file = "st_production_loss_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "formingLossRate", file = "st_forming_loss_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "formingYield", file = "st_forming_yield.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "fabricationYield", file = "st_fabrication_yield.cs4r", aggregate = FALSE)
    # Scrap consumption
    calcOutput("StScrapConsumption", file = "st_scrap_consumption.cs4r", subtype = "assumptions")
    calcOutput("StScrapConsumption", file = "st_scrap_consumption_no_assumptions.cs4r", subtype = "noAssumptions", warnNA = FALSE)
    # Pig Iron
    calcOutput("StPigIronProduction", file = "st_pig_iron_production.cs4r")
    calcOutput("StPigIronTrade", file = "st_pig_iron_imports.cs4r", subtype = "imports")
    calcOutput("StPigIronTrade", file = "st_pig_iron_exports.cs4r", subtype = "exports")
    # Direct reduced Iron
    calcOutput("StDRIData", file = "st_dri_production.cs4r", subtype = "production")
    calcOutput("StDRIData", file = "st_dri_imports.cs4r", subtype = "imports")
    calcOutput("StDRIData", file = "st_dri_exports.cs4r", subtype = "exports")
  }

  #  ------------- CEMENT -----------
  if (runSection("cement")) {
    # Production
    calcOutput("CeBinderProduction", file = "ce_cement_production.cs4r", years = 1900:2023, subtype = "cement")
    # Trade
    calcOutput("CeMaterialTrade", file = "ce_cement_trade.cs4r", years = 1900:2023, subtype = "cement")
    calcOutput("CeMaterialTrade", file = "ce_clinker_trade.cs4r", years = 1900:2023, subtype = "clinker")
    # Parameters
    calcOutput("CeBuiltLifespan", file = "ce_use_lifetime_mean.cs4r")
    calcOutput("CeClinkerRatio", file = "ce_clinker_ratio.cs4r", years = 1900:2023)
    # Service demand / bottom-up
    calcOutput("CeBuildingsMI", file = "ce_ConcreteBuildingMI.cs4r", subtype = "concrete")
    calcOutput("CeBuildingSplit", file = "ce_BuildingSplit.cs4r")
    calcOutput("CeBuildingSplit", file = "ce_BuildingSplit.cs4r")
  }

  #  ------------- PLASTIC -----------
  if (runSection("plastic")) {
    # Consumption
    calcOutput("PlConsumptionByGood", file = "pl_consumption.cs4r")
    # Trade
    calcOutput("PlTrade", category = "final", flow_label = "Exports", file = "pl_final_his_exports.cs4r")
    calcOutput("PlTrade", category = "final", flow_label = "Imports", file = "pl_final_his_imports.cs4r")
    calcOutput("PlTrade", category = "primary", flow_label = "Exports", file = "pl_primary_his_exports.cs4r")
    calcOutput("PlTrade", category = "primary", flow_label = "Imports", file = "pl_primary_his_imports.cs4r")
    calcOutput("PlTrade", category = "intermediate", flow_label = "Exports", file = "pl_intermediate_his_exports.cs4r")
    calcOutput("PlTrade", category = "intermediate", flow_label = "Imports", file = "pl_intermediate_his_imports.cs4r")
    calcOutput("PlTrade", category = "manufactured", flow_label = "Exports", file = "pl_manufactured_his_exports.cs4r")
    calcOutput("PlTrade", category = "manufactured", flow_label = "Imports", file = "pl_manufactured_his_imports.cs4r")
    calcOutput("PlWasteTrade", subtype = "export", file = "pl_waste_exports.cs4r")
    calcOutput("PlWasteTrade", subtype = "import", file = "pl_waste_imports.cs4r")
    # Parameters
    calcOutput("PlOECD_MGshare", file = "pl_material_shares_in_goods.cs4r")
    calcOutput("PlMechReYield", round = 2, file = "pl_mechanical_recycling_yield.cs4r") # fix 0.79
    calcOutput("PlMechLoss", file = "pl_reclmech_loss_uncontrolled_rate.cs4r") # fix 0.05
    calcOutput("PlLifetime", subtype = "Lifetime_mean", aggregate = FALSE, file = "pl_lifetime_mean.cs4r")
    calcOutput("PlLifetime", subtype = "Lifetime_std", aggregate = FALSE, file = "pl_lifetime_std.cs4r")
    # Historic EoL shares
    calcOutput("PlEoL_shares", subtype = "Collected", file = "pl_hist_collection_rate.cs4r")
    calcOutput("PlEoL_shares", subtype = "Recycled", file = "pl_hist_mechanical_recycling_rate.cs4r")
    calcOutput("PlEoL_shares", subtype = "Incinerated", file = "pl_hist_incineration_rate.cs4r")
    # EoL shares including extrapolations (to be moved to the MFA soon)
    calcOutput("PlCollRate", file = "pl_collection_rate.cs4r")
    calcOutput("PlMechReRate", file = "pl_mechanical_recycling_rate.cs4r")
    calcOutput("PlIncinRate", file = "pl_incineration_rate.cs4r")
    # Future rates (historic = 0)
    calcOutput("PlChemReRate", file = "pl_chemical_recycling_rate.cs4r")
    calcOutput("PlBioRate", file = "pl_bio_production_rate.cs4r")
    calcOutput("PlDACRate", file = "pl_daccu_production_rate.cs4r")
    # TODO
    # carbon content materials, and emission capture rate
  }
}
