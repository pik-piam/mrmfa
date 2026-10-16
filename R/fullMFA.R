#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the
#' MFA model.
#'
#' @author Merlin HOSAK
#' @author Bennet Weiss
#' @param rev Revision number for the data version
#' @param dev Development version string
#' @param gdpPerCapita bool if GDP should be returned as per capita values.
#' @param driverScenarios Character vector or string specifying the scenarios to use for GDP and population data.
#' @param runSections Character vector or string selecting which parts to run.
#' Allowed values (see validSections): c("steel", "cement", "plastic"). NULL (default) runs all.
#' @param end_future End year for future data (default: 2100).
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}},
#' \code{\link[madrat]{calcOutput}}
#' @examples
#' \dontrun{
#' retrieveData("MFA")
#' fullMFA()
#' }
#'
fullMFA <- function(rev = 0,
                    dev = "",
                    gdpPerCapita = TRUE,
                    driverScenarios = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"),
                    runSections = NULL,
                    end_future = 2100) {
  # prepare section selector
  validSections <- c("steel", "cement", "plastic")

  if (is.null(runSections)) {
    runSections <- validSections
  } else {
    bad <- setdiff(runSections, validSections)
    if (length(bad)) warning("Invalid sections: ", paste(bad, collapse = ", "))
    runSections <- intersect(runSections, validSections)
  }

  runSection <- function(name) name %in% runSections

  # nolint start

  #  ------------- STEEL ----------------
  if (runSection("steel")) {
    start_historical <- 1900
    end_historical <- 2022

    # common parameters
    calcOutput("CoPopulation", file = "st_population.cs4r", scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = start_historical:end_future)
    calcOutput("CoGDP", file = "st_gdppc.cs4r", perCapita = gdpPerCapita, scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = start_historical:end_future)

    # Production
    calcOutput("StProduction", file = "st_steel_production.cs4r", years = start_historical:end_historical)
    # calcOutput("StProductionByProcess", file = "st_steel_production_by_process.cs4r")

    # Trade
    calcOutput("StTrade", file = "st_steel_imports.cs4r", subtype = "imports", category = "steel", target_years = start_historical:end_historical)
    calcOutput("StTrade", file = "st_steel_exports.cs4r", subtype = "exports", category = "steel", target_years = start_historical:end_historical)
    calcOutput("StTrade", file = "st_scrap_imports.cs4r", subtype = "imports", category = "scrap", target_years = start_historical:end_historical)
    calcOutput("StTrade", file = "st_scrap_exports.cs4r", subtype = "exports", category = "scrap", target_years = start_historical:end_historical)
    calcOutput("StTrade", file = "st_manufactured_products_imports.cs4r", subtype = "imports", category = "manufactured", target_years = start_historical:end_historical)
    calcOutput("StTrade", file = "st_manufactured_products_exports.cs4r", subtype = "exports", category = "manufactured", target_years = start_historical:end_historical)

    # Parameters
    calcOutput("StCullenFabricationYield", file = "st_manufacturing_yield.cs4r", aggregate = FALSE)
    calcOutput("StLifetimes", subtype = "Cooper2014", unit = "mean", file = "st_lifetime_mean.cs4r", aggregate = FALSE)
    calcOutput("StLifetimes", subtype = "Cooper2014", unit = "std", file = "st_lifetime_std.cs4r", aggregate = FALSE)
    calcOutput("StRecoveryRate", subtype = "WorldSteel", file = "st_recovery_rate.cs4r", aggregate = FALSE)
    calcOutput("StSectorSplits", subtype = "high", file = "st_end_use_split_high_inco.cs4r", aggregate = FALSE)
    calcOutput("StSectorSplits", subtype = "low", file = "st_end_use_split_low_inco.cs4r", aggregate = FALSE)

    # Static Parameters
    # calcOutput("StMaxScrapShare", subtype = "BIR", file = "st_max_scrap_share.cs4r", aggregate = FALSE)
    # calcOutput("StWorldSteelStaticParameters", subtype = "scrapInBOFrate", file = "st_scrap_in_bof_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "productionLossRate", file = "st_steel_production_loss_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "formingLossRate", file = "st_forming_loss_rate.cs4r", aggregate = FALSE)
    calcOutput("StCullenStaticParameters", subtype = "formingYield", file = "st_forming_yield.cs4r", aggregate = FALSE)

    # Scrap consumption
    calcOutput("StScrapConsumption", file = "st_scrap_consumption_no_assumptions.cs4r", subtype = "noAssumptions", warnNA = FALSE, years = start_historical:end_historical)
    calcOutput("StScrapConsumption", file = "st_scrap_consumption.cs4r", subtype = "assumptions", years = start_historical:end_historical)

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
    start_historical <- 1900
    end_historical <- 2023

    # common parameters
    calcOutput("CoPopulation", file = "ce_population.cs4r", scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = start_historical:end_future)
    calcOutput("CoGDP", file = "ce_gdppc.cs4r", perCapita = gdpPerCapita, scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = start_historical:end_future)
    # Production
    calcOutput("CeBinderProduction", file = "ce_cement_production.cs4r", years = start_historical:end_historical, subtype = "cement")
    # Trade [Warning: years argument does not work properly. Set target_years instead.]
    calcOutput("CeTrade", file = "ce_cement_imports.cs4r", subtype = "Imports", category = "cement", target_years = start_historical:end_historical)
    calcOutput("CeTrade", file = "ce_cement_exports.cs4r", subtype = "Exports", category = "cement", target_years = start_historical:end_historical)
    calcOutput("CeTrade", file = "ce_clinker_imports.cs4r", subtype = "Imports", category = "clinker", target_years = start_historical:end_historical)
    calcOutput("CeTrade", file = "ce_clinker_exports.cs4r", subtype = "Exports", category = "clinker", target_years = start_historical:end_historical)
    # Parameters
    calcOutput("CeBuiltLifespan", file = "ce_lifetime_mean.cs4r", years = start_historical:end_historical)
    calcOutput("CeLifetimeRelStd", file = "ce_lifetime_rel_std.cs4r", aggregate = FALSE)
    calcOutput("CeClinkerRatio", file = "ce_clinker_ratio.cs4r", years = start_historical:end_historical)
    calcOutput("CeCementRatio", file = "ce_cement_ratio.cs4r")
    calcOutput("CeCementLosses", file = "ce_cement_losses.cs4r", subtype = "cement_loss_construction", aggregate = FALSE)
    calcOutput("CeCementLosses", file = "ce_clinker_losses.cs4r", subtype = "clinker_loss_production", aggregate = FALSE)
    calcOutput("CeProductMaterialSplit", file = "ce_product_material_split.cs4r")
    calcOutput("CeEndUseSplit", file = "ce_end_use_split.cs4r")
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
    calcOutput("CeMaterialApplicationSplit", file = "ce_material_application_split.cs4r")
    calcOutput("CeWasteSizeBound", file = "ce_waste_size_min.cs4r", subtype = "min", aggregate = FALSE)
    calcOutput("CeWasteSizeBound", file = "ce_waste_size_max.cs4r", subtype = "max", aggregate = FALSE)
    # Service demand / bottom-up
    calcOutput("CeFloorspaceEDGEB", file = "ce_floorspace.cs4r", scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = 1990:end_future)
    calcOutput("CeBuildingsMI", file = "ce_concrete_building_mi.cs4r", subtype = "concrete")
    calcOutput("CeDwellingSplit", file = "ce_dwelling_split.cs4r")
    calcOutput("CeStructureSplit", file = "ce_structure_split.cs4r")
    calcOutput("CeHibernatingStockShare", file = "ce_hibernating_stock_share.cs4r")
  }

  #  ------------- PLASTIC -----------
  if (runSection("plastic")) {
    start_historical <- 1950
    end_historical <- 2024

    # common parameters
    calcOutput("CoPopulation", file = "pl_population.cs4r", scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = start_historical:end_future)
    calcOutput("CoGDP", file = "pl_gdppc.cs4r", perCapita = gdpPerCapita, scenarios = driverScenarios, collapse = FALSE, smooth = TRUE, years = start_historical:end_future)
    # Production
    calcOutput("PlProduction", file = "pl_production.cs4r", years = start_historical:end_historical)
    # Consumption
    calcOutput("PlSectorPolymerSplit", file = "pl_end_use_polymer_split.cs4r", target_years = start_historical:end_historical)
    # Trade
    calcOutput("PlTrade", category = "Application", flow_label = "Exports", data_source = "BACI_UNEP", file = "pl_manufactured_products_exports.cs4r", target_years = start_historical:end_historical)
    calcOutput("PlTrade", category = "Application", flow_label = "Imports", data_source = "BACI_UNEP", file = "pl_manufactured_products_imports.cs4r", target_years = start_historical:end_historical)
    calcOutput("PlTrade", category = "Primary", flow_label = "Exports", data_source = "BACI_UNEP", file = "pl_primary_exports.cs4r", target_years = start_historical:end_historical)
    calcOutput("PlTrade", category = "Primary", flow_label = "Imports", data_source = "BACI_UNEP", file = "pl_primary_imports.cs4r", target_years = start_historical:end_historical)
    calcOutput("PlTrade", category = "Waste", flow_label = "Exports", data_source = "BACI_UNEP", file = "pl_waste_exports.cs4r", target_years = start_historical:end_historical)
    calcOutput("PlTrade", category = "Waste", flow_label = "Imports", data_source = "BACI_UNEP", file = "pl_waste_imports.cs4r", target_years = start_historical:end_historical)
    # Parameters
    calcOutput("PlHVCinput", subtype = "polymerization_yield", aggregate = FALSE, file = "pl_polymerization_yield.cs4r")
    calcOutput("PlHVCinput", subtype = "HVC_input_ratio", aggregate = FALSE, file = "pl_HVC_input_ratio.cs4r")
    calcOutput("PlHVCinput", subtype = "C4_input_ratio", aggregate = FALSE, file = "pl_C4_input_ratio.cs4r")
    calcOutput("PlMechReYield", round = 2, file = "pl_mechanical_recycling_yield.cs4r", aggregate = FALSE) # fix 0.79
    calcOutput("PlMechLoss", file = "pl_reclmech_loss_uncontrolled_rate.cs4r", aggregate = FALSE) # fix 0.05
    calcOutput("PlLifetime", subtype = "Lifetime_mean", aggregate = FALSE, file = "pl_lifetime_mean.cs4r")
    calcOutput("PlLifetime", subtype = "Lifetime_std", aggregate = FALSE, file = "pl_lifetime_std.cs4r")
    calcOutput("PlCarbonContent", aggregate = FALSE, file = "pl_carbon_content_materials.cs4r")
    # Historical EoL shares
    calcOutput("PlEoL_shares", subtype = "Collected", file = "pl_collection_rate.cs4r", years = start_historical:end_historical)
    calcOutput("PlEoL_shares", subtype = "Recycled", file = "pl_mechanical_recycling_rate.cs4r", years = start_historical:end_historical)
    calcOutput("PlEoL_shares", subtype = "Incinerated", file = "pl_incineration_rate.cs4r", years = start_historical:end_historical)
    # Rates that are historically zero
    calcOutput("PlZeroRates", file = "pl_chemical_recycling_rate.cs4r", aggregate = FALSE)
    calcOutput("PlZeroRates", file = "pl_bio_production_rate.cs4r", aggregate = FALSE)
    calcOutput("PlZeroRates", file = "pl_daccu_production_rate.cs4r", aggregate = FALSE)
    calcOutput("PlZeroRates", file = "pl_emission_capture_rate.cs4r", aggregate = FALSE)
  }

  # nolint end
}
