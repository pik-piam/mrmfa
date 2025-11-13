#' Read World Steel Database
#' @description Read Data from World Steel Association online Database,
#' downloaded to Excel files in the ./v1.0 directory.
#' They should be updated yearly via the PIK's subscription to the database.
#' Most datasets are available between around 2002 and 2022
#' on a yearly resolution.
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
readWorldSteelDatabase <- function(subtype = "production") {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "production" = function() {
      x <- readWSDatabaseStandard("P01_crude_2023-10-23")
      return(x)
    },
    "bofProduction" = function() {
      x <- readWSDatabaseStandard("P05_bof_2023-10-23")
      return(x)
    },
    "eafProduction" = function() {
      x <- readWSDatabaseStandard("P06_eaf_2023-10-23")
      return(x)
    },
    "imports" = function() {
      x <- readWSDatabaseStandard("T02_imports_finished-2023-10-23")
      return(x)
    },
    "exports" = function() {
      x <- readWSDatabaseStandard("T01_exports_finished-2023-10-23")
      return(x)
    },
    "scrapImports" = function() {
      x <- readWSDatabaseStandard("T18_imports_scrap-2023-10-23")
      return(x)
    },
    "scrapExports" = function() {
      x <- readWSDatabaseStandard("T17_exports_scrap-2023-10-23")
      return(x)
    },
    "indirectImports" = function() {
      x <- readWSDatabaseStandard("I02_indirect_imports_2023-10-23")
      x <- adaptWSDatabaseIndirectTrade(x)
      return(x)
    },
    "indirectExports" = function() {
      x <- readWSDatabaseStandard("I01_indirect_exports_2023-10-23")
      x <- adaptWSDatabaseIndirectTrade(x)
      return(x)
    },
    "pigIronProduction" = function() {
      x <- readWSDatabaseStandard("P26_pigiron_2023-10-23")
      return(x)
    },
    "pigIronImports" = function() {
      x <- readWSDatabaseStandard("T12_imports_pigiron-2023-10-23")
      return(x)
    },
    "pigIronExports" = function() {
      x <- readWSDatabaseStandard("T11_exports_pigiron-2023-10-23")
      return(x)
    },
    "driProduction" = function() {
      x <- readWSDatabaseStandard("P27_driron_2023-10-23")
      return(x)
    },
    "driImports" = function() {
      x <- readWSDatabaseStandard("T14_imports_driron-2023-10-23")
      return(x)
    },
    "driExports" = function() {
      x <- readWSDatabaseStandard("T13_exports_driron-2023-10-23")
      return(x)
    }
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}


readWSDatabaseStandard <- function(name, version = "1.0") {
  # read data from Excel file
  path <- paste0("./v", version, "/", name, ".xlsx")

  # Read the excel and suppress warning about non-numeric values in B column
  # as WS files are always in the same format, last rows will be cut anyways.
  x <- readxl::read_excel(path, skip = 2) %>% suppressSpecificWarnings(
    regularExpr = "Expecting numeric in B"
  )

  # delete last 5 rows as they are irrelevant in WS Database files
  x <- x[1:(nrow(x) - 5), ]


  # Delete Others and World rows
  x <- x[!x$Country %in% c("Others", "World"), ]

  # convert to magpie object
  x <- as.magpie(x, spatial = "Country")

  x <- toolFoo(x)

  x <- x * 1e3 # convert from kt to tonnes

  return(x)
}

adaptWSDatabaseIndirectTrade <- function(x) {
  # distribute Belgium Luxemburg 80/20 %

  # add new country row for country bellux
  x <- add_columns(x, addnm = c("BEL", "LUX", "SRB", "MNE"), dim = 1)

  x["BEL", ] <- x["BLX", ] * 0.8
  x["LUX", ] <- x["BLX", ] * 0.2
  x <- x[-which(rownames(x) == "BLX"), ]

  x["SRB", ] <- x["SCG", ] * 0.9
  x["MNE", ] <- x["SCG", ] * 0.1
  x <- x[-which(rownames(x) == "SCG"), ]

  return(x)
}
