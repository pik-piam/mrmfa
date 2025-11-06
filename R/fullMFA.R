#' Load all MFA data
#' @description
#' Function that produces the complete regional data sets required for the 
#' MFA model.
#'
#' @author Merlin HOSAK
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}}, 
#' \code{\link[madrat]{calcOutput}}, \code{\link[mrindustry]{calcSteelProduction}}
#' @export
#' @examples
#' \dontrun{
#' retrieveData('MFA')
#' fullMFA()
#' }
#'
fullMFA <- function(scenario='SSP2', gdpPerCapita=FALSE) {
  
  #  ------------- DRIVERS -------------
  calcOutput("Population1900To2150", file = "population1900To2150.cs3r", 
             scenario=scenario)
  calcOutput("GDP1900To2150", file = "gdp1900To2150.cs3r", 
             scenario=scenario, 
             perCapita=gdpPerCapita)
  
  #  ------------- STEEL ----------------
  
    # Production
  
  calcOutput("SteelProduction", file = "steel_production.cs3r")
  calcOutput("SteelProductionByProcess", file = "steel_production_by_process.cs3r")
  
    # Trade
  
  calcOutput("SteelTrade", file = "steel_imports.cs3r", subtype='imports')
  calcOutput("SteelTrade", file = "steel_exports.cs3r", subtype='exports')
  calcOutput("SteelTrade", file = "steel_scrap_imports.cs3r", subtype='scrapImports')
  calcOutput("SteelTrade", file = "steel_scrap_exports.cs3r", subtype='scrapExports')
  calcOutput("SteelTrade", file = "steel_indirect_imports.cs3r", subtype='indirectImports')
  calcOutput("SteelTrade", file = "steel_indirect_exports.cs3r", subtype='indirectExports')
  
    # Parameters
   
  calcOutput("CullenFabricationYield", file = "fabrication_yield.cs3r", aggregate=F)
  calcOutput("SteelLifetimes", subtype='Cooper2014', file = "lifetimes.cs3r", aggregate=F)
  calcOutput("SteelRecoveryRate", subtype='WorldSteel', file = "recovery_rate.cs3r", aggregate=F)
  calcOutput("SteelSectorSplits", subtype='Pauliuk2013', file = "sector_splits.cs3r", aggregate=F)
  
    # Static Parameters
  
  calcOutput('SteelMaxScrapShare', subtype='BIR', file='max_scrap_share.cs3r', aggregate=F)
  calcOutput('WorldSteelStaticParameters', subtype='scrapInBOFrate', file='scrap_in_BOF_rate.cs3r', aggregate=F)
  calcOutput('CullenStaticParameters', subtype='productionLossRate', file='production_loss_rate.cs3r', aggregate=F)
  calcOutput('CullenStaticParameters', subtype='formingLossRate', file='forming_loss_rate.cs3r', aggregate=F)
  calcOutput('CullenStaticParameters', subtype='formingYield', file='forming_yield.cs3r', aggregate = F)
  calcOutput('CullenStaticParameters', subtype='fabricationYield', file='fabrication_yield.cs3r', aggregate = F)
  
    # Scrap consumption
  
  calcOutput("SteelScrapConsumption", file = "steel_scrap_consumption.cs3r", subtype='assumptions')
  calcOutput('SteelScrapConsumption', file = "steel_scrap_consumption_no_assumptions.cs3r", subtype='noAssumptions', warnNA=F)
  
    # Pig Iron
  
  calcOutput("PigIronProduction", file = "pig_iron_production.cs3r")
  calcOutput("PigIronTrade", file = "pig_iron_imports.cs3r", subtype='imports')
  calcOutput("PigIronTrade", file = "pig_iron_exports.cs3r", subtype='exports')
  
    # Direct reduced Iron
  
  calcOutput("DRIData", file = "dri_production.cs3r", subtype='production')
  calcOutput("DRIData", file = "dri_imports.cs3r", subtype='imports')
  calcOutput("DRIData", file = "dri_exports.cs3r", subtype='exports')
  
  #  ------------- CEMENT -----------
  
}