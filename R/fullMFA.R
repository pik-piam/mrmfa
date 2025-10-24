#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the
#' MFA model.
#' @param run_sections Character vector selecting which parts to run.
#'   Allowed: c("drivers","steel","cement"). NULL (default) runs all.
#'
#' @author Merlin HOSAK
#' @author Bennet Weiss
#' @param rev TODOMERLIN: document
#' @param dev TODOMERLIN: document
#' @param scenario TODOMERLIN: document
#' @param gdp_per_capita TODOMERLIN: document
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}},
#' \code{\link[madrat]{calcOutput}}
#' @export
#' @examples
#' \dontrun{
#' retrieveData('MFA')
#' fullMFA()
#' }
#'
fullMFA <- function(rev = 0, dev = "", scenario='SSP2', gdp_per_capita=FALSE, run_sections=NULL) {

  # prepare section selector
  validSections <- c("drivers","steel","cement","plastic")

  if (is.null(run_sections)) {
    run_sections <- validSections
  } else {
    bad <- setdiff(run_sections, validSections)
    if (length(bad)) stop("Invalid sections: ", paste(bad, collapse=", "))
  }

  runSection <- function(name) name %in% run_sections

  if (!length(run_sections)) {
    message("fullMFA: no sections selected; nothing done.")
    return(invisible(NULL))
  }

  #  ------------- DRIVERS -------------
  if (runSection("drivers")) {
    calcOutput("Population1900To2150", file = "co_population1900To2150.cs4r", scenario=scenario)
    calcOutput("GDP1900To2150", file = "co_gdp1900To2150.cs4r", scenario=scenario, per_capita=gdp_per_capita)
  }

  #  ------------- STEEL ----------------
  if (runSection("steel")) {
    # Production
    calcOutput("SteelProduction", file = "st_steel_production.cs4r")
    # Trade
    calcOutput("SteelTrade", file = "st_steel_imports.cs4r", subtype='imports')
    calcOutput("SteelTrade", file = "st_steel_exports.cs4r", subtype='exports')
    calcOutput("SteelTrade", file = "st_steel_scrap_imports.cs4r", subtype='scrap_imports')
    calcOutput("SteelTrade", file = "st_steel_scrap_exports.cs4r", subtype='scrap_exports')
    calcOutput("SteelTrade", file = "st_steel_indirect_imports.cs4r", subtype='indirect_imports')
    calcOutput("SteelTrade", file = "st_steel_indirect_exports.cs4r", subtype='indirect_exports')
    # Parameters
    calcOutput("SteelStaticParameters", file = "st_steel_static_parameters.cs4r", aggregate=FALSE)
    calcOutput("CullenFabricationYield", file = "st_fabrication_yield.cs4r", aggregate=FALSE)
    calcOutput("SteelLifetimes", subtype='Cooper2014', file = "st_lifetimes.cs4r", aggregate=FALSE)
    calcOutput("SteelRecoveryRate", subtype='WorldSteel', file = "st_recovery_rate.cs4r", aggregate=FALSE)
    calcOutput("SteelSectorSplits", subtype='Pauliuk2013', file = "st_sector_splits.cs4r", aggregate=FALSE)
  }

  #  ------------- CEMENT -----------
  if (runSection("cement")) {
    # Production
    calcOutput("CeBinderProduction", file = "ce_cement_production.cs4r", years=1900:2023, subtype="cement")
    # Trade
    calcOutput("CeMaterialTrade", file = "ce_cement_trade.cs4r", years=1900:2023, subtype="cement")
    calcOutput("CeMaterialTrade", file = "ce_clinker_trade.cs4r", years=1900:2023, subtype="clinker")
    # Parameters
    calcOutput("CeBuiltLifespan", file = "ce_use_lifetime_mean.cs4r")
    calcOutput("CeClinkerRatio", file = "ce_clinker_ratio.cs4r", years=1900:2023)
  }

  #  ------------- PLASTIC -----------
  if (runSection("plastic")) {
    # Consumption
    calcOutput("PlConsumptionByGood", file = "pl_consumption.cs4r")
    # Trade
    calcOutput("PlTrade",category = "final", flow_label = "Exports",file = "pl_final_his_exports.cs4r")
    calcOutput("PlTrade",category = "final", flow_label = "Imports",file = "pl_final_his_imports.cs4r")
    calcOutput("PlTrade",category = "primary", flow_label = "Exports",file = "pl_primary_his_exports.cs4r")
    calcOutput("PlTrade",category = "primary", flow_label = "Imports",file = "pl_primary_his_imports.cs4r")
    calcOutput("PlTrade",category = "intermediate", flow_label = "Exports",file = "pl_intermediate_his_exports.cs4r")
    calcOutput("PlTrade",category = "intermediate", flow_label = "Imports",file = "pl_intermediate_his_imports.cs4r")
    calcOutput("PlTrade",category = "manufactured", flow_label = "Exports",file = "pl_manufactured_his_exports.cs4r")
    calcOutput("PlTrade",category = "manufactured", flow_label = "Imports",file = "pl_manufactured_his_imports.cs4r")
    calcOutput("PlWasteTrade",subtype = "export",file = "pl_waste_exports.cs4r")
    calcOutput("PlWasteTrade",subtype = "import",file = "pl_waste_imports.cs4r")
    # Parameters
    calcOutput("PlOECD_MGshare",file = "pl_material_shares_in_goods.cs4r")
    calcOutput("PlMechReYield",round = 2, file = "pl_mechanical_recycling_yield.cs4r") # fix 0.79
    calcOutput("PlMechLoss",file = "pl_reclmech_loss_uncontrolled_rate.cs4r") # fix 0.05
    calcOutput("PlLifetime", subtype="Lifetime_mean", aggregate=FALSE, file = "pl_lifetime_mean.cs4r")
    calcOutput("PlLifetime", subtype="Lifetime_std", aggregate=FALSE, file = "pl_lifetime_std.cs4r")
    # Historic EoL shares
    calcOutput("PlEoL_shares", subtype="Collected", file = "pl_hist_collection_rate.cs4r")
    calcOutput("PlEoL_shares", subtype="Recycled", file = "pl_hist_mechanical_recycling_rate.cs4r")
    calcOutput("PlEoL_shares", subtype="Incinerated", file = "pl_hist_incineration_rate.cs4r")
    # EoL shares including extrapolations (to be moved to the MFA soon)
    calcOutput("PlCollRate", file = "pl_collection_rate.cs4r")
    calcOutput("PlMechReRate", file = "pl_mechanical_recycling_rate.cs4r")
    calcOutput("PlIncinRate", file = "pl_incineration_rate.cs4r")
    # Future rates (historic = 0)
    calcOutput("PlChemReRate",file = "pl_chemical_recycling_rate.cs4r")
    calcOutput("PlBioRate",file = "pl_bio_production_rate.cs4r")
    calcOutput("PlDACRate",file = "pl_daccu_production_rate.cs4r")
    # TODO
    # carbon content materials, and emission capture rate
  }

}
