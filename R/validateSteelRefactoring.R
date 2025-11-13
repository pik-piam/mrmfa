foo <- function() {
  setConfig(cachefolder = "~/madrat/cache/clean-cache/", forcecache = F, ignorecache = T)

  # WorldSteelDigitised ----
  prodHist <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
  prodRecent <- readSource("WorldSteelDigitised", subtype = "production")
  worldScrapConsumption <- readSource("WorldSteelDigitised", subtype = "worldScrapConsumption", convert = FALSE)

  # only works without convert (maybe refactor?)
  bofRecent <- readSource("WorldSteelDigitised", subtype = "bofProduction", convert = FALSE)
  eafRecent <- readSource("WorldSteelDigitised", subtype = "eafProduction", convert = FALSE)
  byProcess <- readSource("WorldSteelDigitised", subtype = "productionByProcess", convert = FALSE)
  scrapConsumptionYearbooks <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionYearbooks", convert = FALSE)
  scrapConsumptionFigures <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionFigures", convert = FALSE)
  specificScrapConsumption70s <- readSource("WorldSteelDigitised", subtype = "specificScrapConsumption70s", convert = FALSE)

  # not used right now?
  imports <- readSource("WorldSteelDigitised", subtype = "imports", convert = FALSE)
  exports <- readSource("WorldSteelDigitised", subtype = "exports", convert = FALSE)
  scrapImports <- readSource("WorldSteelDigitised", subtype = "scrapImports", convert = FALSE)
  scrapExports <- readSource("WorldSteelDigitised", subtype = "scrapExports", convert = FALSE)

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

}

