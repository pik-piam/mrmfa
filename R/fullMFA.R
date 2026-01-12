#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the
#' MFA model.
#'
#' @author Merlin HOSAK
#' @author Bennet Weiss
#' @param rev Revision number for the data version
#' @param dev Development version string
#' @param scenario SSP scenario used for population and GDP.
#' @param gdpPerCapita bool if GDP should be returned as per capita values.
#' @param runSections Character vector selecting which parts to run.
#' Allowed values (see validSections): c("steel", "cement", "plastic"). NULL (default) runs all.
#' #' @param end_future End year for future data (default: 2100).
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}},
#' \code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{
#' retrieveData("MFA")
#' fullMFA()
#' }
#'
fullMFA <- function(rev = 0, dev = "", scenario = "SSP2", gdpPerCapita = TRUE, runSections = NULL,
                    end_future = 2100) {
  # prepare section selector
  validSections <- c("steel", "cement", "plastic")

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

  # nolint start

  #  ------------- STEEL ----------------
  if (runSection("steel")) {
    start_historic <- 1900
    end_historic <- 2022

    # common parameters
    calcOutput("CoPopulation1900To2150", file = "st_population.cs4r", scenario = scenario, years = start_historic:end_future)
    calcOutput("CoGDP1900To2150", file = "st_gdppc.cs4r", scenario = scenario, perCapita = gdpPerCapita, years = start_historic:end_future)

    # Production
    calcOutput("StProduction", file = "st_production.cs4r", years = start_historic:end_historic)
    # calcOutput("StProductionByProcess", file = "st_steel_production_by_process.cs4r")

    # Trade
    calcOutput("StTrade", file = "st_steel_imports.cs4r", subtype = "imports", years = start_historic:end_historic)
    calcOutput("StTrade", file = "st_steel_exports.cs4r", subtype = "exports", years = start_historic:end_historic)
    calcOutput("StTrade", file = "st_scrap_imports.cs4r", subtype = "scrapImports", years = start_historic:end_historic)
    calcOutput("StTrade", file = "st_scrap_exports.cs4r", subtype = "scrapExports", years = start_historic:end_historic)
    calcOutput("StTrade", file = "st_indirect_imports.cs4r", subtype = "indirectImports", years = start_historic:end_historic)
    calcOutput("StTrade", file = "st_indirect_exports.cs4r", subtype = "indirectExports", years = start_historic:end_historic)

    # Parameters
    calcOutput("StCullenFabricationYield", file = "st_fabrication_yield.cs4r", aggregate = FALSE)
    calcOutput("StLifetimes", subtype = "Cooper2014", unit = "mean", file = "st_lifetime_mean.cs4r", aggregate = FALSE)
    calcOutput("StLifetimes", subtype = "Cooper2014", unit = "std", file = "st_lifetime_std.cs4r", aggregate = FALSE)
    calcOutput("StRecoveryRate", subtype = "WorldSteel", file = "st_recovery_rate.cs4r", aggregate = FALSE)
    calcOutput("StSectorSplits", subtype = "high", file = "st_sector_split_high.cs4r", aggregate = FALSE)
    calcOutput("StSectorSplits", subtype = "low", file = "st_sector_split_low.cs4r", aggregate = FALSE)

    # Static Parameters
    # calcOutput("StMaxScrapShare", subtype = "BIR", file = "st_max_scrap_share.cs4r", aggregate = FALSE)
    calcOutput("StWorldSteelStaticParameters", subtype = "scrapInBOFrate", file = "st_scrap_in_BOF_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "productionLossRate", file = "st_production_loss_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "formingLossRate", file = "st_forming_loss_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "formingYield", file = "st_forming_yield.cs4r", aggregate = FALSE)

    # Scrap consumption
    calcOutput("StScrapConsumption", file = "st_scrap_consumption_no_assumptions.cs4r", subtype = "noAssumptions", warnNA = FALSE, years = start_historic:end_historic)
    calcOutput("StScrapConsumption", file = "st_scrap_consumption.cs4r", subtype = "assumptions", years = start_historic:end_historic)

    # Pig Iron
    # calcOutput("StPigIronProduction", file = "st_pig_iron_production.cs4r")
    # calcOutput("StPigIronTrade", file = "st_pig_iron_imports.cs4r", subtype = "imports")
    # calcOutput("StPigIronTrade", file = "st_pig_iron_exports.cs4r", subtype = "exports")

    # Direct reduced Iron
    # calcOutput("StDRIData", file = "st_dri_production.cs4r", subtype = "production")
    # calcOutput("StDRIData", file = "st_dri_imports.cs4r", subtype = "imports")
    # calcOutput("StDRIData", file = "st_dri_exports.cs4r", subtype = "exports")
  }

  #  ------------- CEMENT -----------
  if (runSection("cement")) {
    start_historic <- 1900
    end_historic <- 2023

    # common parameters
    calcOutput("CoPopulation1900To2150", file = "ce_population.cs4r", scenario = scenario, years = start_historic:end_future)
    calcOutput("CoGDP1900To2150", file = "ce_gdppc.cs4r", scenario = scenario, perCapita = gdpPerCapita, years = start_historic:end_future)
    # Production
    calcOutput("CeBinderProduction", file = "ce_cement_production.cs4r", years = start_historic:end_historic, subtype = "cement")
    # Trade
    calcOutput("CeMaterialTrade", file = "ce_cement_trade.cs4r", years = start_historic:end_historic, subtype = "cement")
    calcOutput("CeMaterialTrade", file = "ce_clinker_trade.cs4r", years = start_historic:end_historic, subtype = "clinker")
    # Parameters
    calcOutput("CeBuiltLifespan", file = "ce_use_lifetime_mean.cs4r", years = start_historic:end_historic)
    calcOutput("CeClinkerRatio", file = "ce_clinker_ratio.cs4r", years = start_historic:end_historic)
    calcOutput("CeCementLosses", file = "ce_cement_losses.cs4r", subtype = "cement_loss_construction", aggregate = FALSE)
    calcOutput("CeCementLosses", file = "ce_clinker_losses.cs4r", subtype = "clinker_loss_production", aggregate = FALSE)
    calcOutput("CeProductApplicationSplit", file = "ce_product_application_split.cs4r")
    calcOutput("CeProductCementContent", file = "ce_product_cement_content.cs4r", aggregate = FALSE)
    calcOutput("CeProductMaterialSplit", file = "ce_product_material_split.cs4r")
    calcOutput("CeStockTypeSplit", file = "ce_stock_type_split.cs4r")
    calcOutput("CeStockSaturationLevel", file = "ce_stock_saturation_level.cs4r")
    calcOutput("CeIndustrializedRegions", file = "ce_industrialized_regions.cs4r", round = 0)
    calcOutput("CeLifetimeRelStd", file = "ce_use_lifetime_rel_std.cs4r", aggregate = FALSE)
    # Carbonation
    calcOutput("CeCaOCarbonationShare", file = "ce_cao_carbonation_share.cs4r", aggregate = FALSE)
    calcOutput("CeCaOContent", file = "ce_ckd_cao_ratio.cs4r", subtype = "CKD", aggregate = FALSE)
    calcOutput("CeCaOContent", file = "ce_clinker_cao_ratio.cs4r", subtype = "clinker", aggregate = FALSE)
    calcOutput("CeCarbonationRate", file = "ce_carbonation_rate_buried.cs4r", subtype = "base_buried")
    calcOutput("CeCarbonationRate", file = "ce_carbonation_rate_coating.cs4r", subtype = "coating", aggregate = FALSE)
    calcOutput("CeCarbonationRate", file = "ce_carbonation_rate_co2.cs4r", subtype = "co2", aggregate = FALSE)
    calcOutput("CeCarbonationRate", file = "ce_carbonation_rate_additives.cs4r", subtype = "additives", aggregate = FALSE)
    calcOutput("CeCarbonationRate", file = "ce_carbonation_rate.cs4r")
    calcOutput("CeCKDLandfillShare", file = "ce_ckd_landfill_share.cs4r", aggregate = FALSE)
    calcOutput("CeProductThickness", file = "ce_product_thickness.cs4r", aggregate = FALSE)
    calcOutput("CeWasteSizeSplit", file = "ce_waste_size_share.cs4r")
    calcOutput("CeWasteSplit", file = "ce_waste_type_split.cs4r")
    calcOutput("CeCaOEmissionFactor", file = "ce_cao_emission_factor.cs4r", aggregate = FALSE)
    calcOutput("CeProductDensity", file = "ce_product_density.cs4r", aggregate = FALSE)
    calcOutput("CeWasteSizeBound", file = "ce_waste_size_min.cs4r", subtype = "min", aggregate = FALSE)
    calcOutput("CeWasteSizeBound", file = "ce_waste_size_max.cs4r", subtype = "max", aggregate = FALSE)
    calcOutput("CeProductMaterialApplicationTransform", file = "ce_product_material_application_transform.cs4r", aggregate = FALSE)
  }

  #  ------------- PLASTIC -----------
  if (runSection("plastic")) {
    start_historic <- 1950
    end_historic <- 2019

    # common parameters
    calcOutput("CoPopulation1900To2150", file = "pl_population.cs4r", scenario = scenario, years = start_historic:end_future)
    calcOutput("CoGDP1900To2150", file = "pl_gdppc.cs4r", scenario = scenario, perCapita = gdpPerCapita, years = start_historic:end_future)
    # Consumption
    calcOutput("PlConsumptionByGood", file = "pl_consumption.cs4r")
    # Trade
    # calcOutput("PlTrade", category = "Final", flow_label = "Exports", source = "UNCTAD", file = "pl_final_his_exports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Final", flow_label = "Imports", source = "UNCTAD", file = "pl_final_his_imports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Primary", flow_label = "Exports", source = "UNCTAD", file = "pl_primary_his_exports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Primary", flow_label = "Imports", source = "UNCTAD", file = "pl_primary_his_imports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Intermediate", flow_label = "Exports", source = "UNCTAD", file = "pl_intermediate_his_exports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Intermediate", flow_label = "Imports", source = "UNCTAD", file = "pl_intermediate_his_imports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Manufactured", flow_label = "Exports", source = "UNCTAD", file = "pl_manufactured_his_exports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlTrade", category = "Manufactured", flow_label = "Imports", source = "UNCTAD", file = "pl_manufactured_his_imports.cs4r", years = start_historic:end_historic)
    # calcOutput("PlWasteTrade", subtype = "export", file = "pl_waste_exports.cs4r", years = start_historic:end_future)
    # calcOutput("PlWasteTrade", subtype = "import", file = "pl_waste_imports.cs4r", years = start_historic:end_future)
    calcOutput("PlTrade", category = "Application", flow_label = "Exports", source = "BACI", file = "pl_final_his_exports.cs4r", years = start_historic:end_historic)
    calcOutput("PlTrade", category = "Application", flow_label = "Imports", source = "BACI", file = "pl_final_his_imports.cs4r", years = start_historic:end_historic)
    calcOutput("PlTrade", category = "Primary", flow_label = "Exports", source = "BACI", file = "pl_primary_his_exports.cs4r", years = start_historic:end_historic)
    calcOutput("PlTrade", category = "Primary", flow_label = "Imports", source = "BACI", file = "pl_primary_his_imports.cs4r", years = start_historic:end_historic)
    calcOutput("PlTrade", category = "Waste", flow_label = "Exports", source = "BACI", file = "pl_waste_his_exports.cs4r", years = start_historic:end_historic)
    calcOutput("PlTrade", category = "Waste", flow_label = "Imports", source = "BACI", file = "pl_waste_his_imports.cs4r", years = start_historic:end_historic)
    # Parameters
    calcOutput("PlOECD_MGshare", file = "pl_material_shares_in_goods.cs4r")
    calcOutput("PlMechReYield", round = 2, file = "pl_mechanical_recycling_yield.cs4r", years = start_historic:end_future) # fix 0.79
    calcOutput("PlMechLoss", file = "pl_reclmech_loss_uncontrolled_rate.cs4r", years = start_historic:end_future) # fix 0.05
    calcOutput("PlLifetime", subtype = "Lifetime_mean", aggregate = FALSE, file = "pl_lifetime_mean.cs4r")
    calcOutput("PlLifetime", subtype = "Lifetime_std", aggregate = FALSE, file = "pl_lifetime_std.cs4r")
    calcOutput("PlCarbonContent", aggregate = FALSE, file = "pl_carbon_content_materials.cs4r")
    # Historic EoL shares
    calcOutput("PlEoL_shares", subtype = "Collected", file = "pl_collection_rate.cs4r", years = start_historic:end_historic)
    calcOutput("PlEoL_shares", subtype = "Recycled", file = "pl_mechanical_recycling_rate.cs4r", years = start_historic:end_historic)
    calcOutput("PlEoL_shares", subtype = "Incinerated", file = "pl_incineration_rate.cs4r", years = start_historic:end_historic)
    # Rates that are historically zero
    calcOutput("PlZeroRates", file = "pl_chemical_recycling_rate.cs4r", years = start_historic:end_historic)
    calcOutput("PlZeroRates", file = "pl_bio_production_rate.cs4r", years = start_historic:end_historic)
    calcOutput("PlZeroRates", file = "pl_daccu_production_rate.cs4r", years = start_historic:end_historic)
    calcOutput("PlZeroRates", file = "pl_emission_capture_rate.cs4r", years = start_historic:end_historic)
  }

  # nolint end
}
