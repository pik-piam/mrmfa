foo <- function() {
  setConfig(cachefolder = "~/madrat/cache/clean-cache/", forcecache = F, ignorecache = T)

  # WorldSteelDigitised ----
  prodHist <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
  prodRecent <- readSource("WorldSteelDigitised", subtype = "production")
  worldScrapConsumption <- readSource("WorldSteelDigitised", subtype = "worldScrapConsumption", convert = FALSE)

  imports <- readSource("WorldSteelDigitised", subtype = "imports")
  exports <- readSource("WorldSteelDigitised", subtype = "exports")
  scrapImports <- readSource("WorldSteelDigitised", subtype = "scrapImports")
  scrapExports <- readSource("WorldSteelDigitised", subtype = "scrapExports")

  # only works without convert (maybe refactor?)
  bofRecent <- readSource("WorldSteelDigitised", subtype = "bofProduction", convert = FALSE)
  eafRecent <- readSource("WorldSteelDigitised", subtype = "eafProduction", convert = FALSE)
  byProcess <- readSource("WorldSteelDigitised", subtype = "productionByProcess", convert = FALSE)
  scrapConsumptionYearbooks <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionYearbooks", convert = FALSE)
  scrapConsumptionFigures <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionFigures", convert = FALSE)
  specificScrapConsumption70s <- readSource("WorldSteelDigitised", subtype = "specificScrapConsumption70s", convert = FALSE)

  indirectImportsByCategory2013 <- readSource("WorldSteelDigitised", subtype = "indirectImportsByCategory2013", convert = FALSE)
  indirectExportsByCategory2013 <- readSource("WorldSteelDigitised", subtype = "indirectExportsByCategory2013", convert = FALSE)



  # WorldSteelDatabase ----

  production <- readSource("WorldSteelDatabase", subtype = "production")

  bofCurrent <- readSource("WorldSteelDatabase", subtype = "bofProduction")
  eafCurrent <- readSource("WorldSteelDatabase", subtype = "eafProduction")
  imports <- readSource("WorldSteelDatabase", subtype = "imports")
  exports <- readSource("WorldSteelDatabase", subtype = "exports")

  scrap_imports <- readSource("WorldSteelDatabase", subtype = "scrapImports")
  scrap_exports <- readSource("WorldSteelDatabase", subtype = "scrapExports")
  indirectImports <- readSource("WorldSteelDatabase", subtype = "indirectImports")
  indirectExports <- readSource("WorldSteelDatabase", subtype = "indirectExports")

  pigIronProduction <- readSource("WorldSteelDatabase", subtype = "pigIronProduction")
  pigIronImports <- readSource("WorldSteelDatabase", subtype = "pigIronImports")

  pigIronExports <- readSource("WorldSteelDatabase", subtype = "pigIronExports")
  driProduction <- readSource("WorldSteelDatabase", subtype = "driProduction")
  driImports <- readSource("WorldSteelDatabase", subtype = "driImports")
  driExports <- readSource("WorldSteelDatabase", subtype = "driExports")


  ##

  renv::install("leonieschweiger/mrmfa@steel_updates")
  renv::install("Merjo/madrat@simson-country-map")

  library(madrat)
  library(mrmfa)
  setConfig(outputfolder = "/p/tmp/benke/steel_before")

  # Production
  calcOutput("StProduction", file = "st_steel_production.cs4r")

  # Trade
  calcOutput("StTrade", file = "st_steel_imports.cs4r", subtype = "imports")
  calcOutput("StTrade", file = "st_steel_exports.cs4r", subtype = "exports")
  calcOutput("StTrade", file = "st_steel_scrap_imports.cs4r", subtype = "scrapImports")
  calcOutput("StTrade", file = "st_steel_scrap_exports.cs4r", subtype = "scrapExports")
  calcOutput("StTrade", file = "st_steel_indirect_imports.cs4r", subtype = "indirectImports")
  calcOutput("StTrade", file = "st_steel_indirect_exports.cs4r", subtype = "indirectExports")

  piamutils::compareMagpieObject("~/madrat/output/steel_before/st_steel_indirect_imports.cs4r", "~/madrat/output/new/st_steel_indirect_imports.cs4r")


}

