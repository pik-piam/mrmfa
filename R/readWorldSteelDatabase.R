#' Read World Steel Database
#' @description Read Data from World Steel Association online Database,
#' downloaded to Excel files in the ./v1.0 directory.
#' They should be updated yearly via the PIK's subscription to the database.
#' Most datasets are available between around 2002 and 2022
#' on a yearly resolution.
#' @author Merlin Jo Hosak
#' @param subtype must be one of "production", "bofProduction", "eafProduction",
#' "imports", "exports", "scrapImports",  "scrapExports", "indirectImports",
#' "indirectExports", "pigIronProduction", "pigIronImports", "pigIronExports",
#' "driProduction", "driImports", "driExports"
#'
#'
#' TODO: check if all these subtpyes are actually used
readWorldSteelDatabase <- function(subtype = "production") {
  .readCommonSourceFormat <- function(name, version = "v1.0") {

    # read data from Excel file
    path <- file.path(".", version, name)

    # Read the excel and suppress warning about non-numeric values in B column
    # as WS files are always in the same format, last rows will be cut anyways.
    x <- readxl::read_excel(path, skip = 2) %>% suppressSpecificWarnings(
      regularExpr = "Expecting numeric in B"
    )

    # delete last 5 rows as they are irrelevant in WS Database files
    x <- x[1:(nrow(x) - 5), ]

    x <- x %>%
      tidyr::pivot_longer(c(-"Country"), names_to = "period") %>%
      dplyr::rename("country_name" = "Country") %>%
      toolCleanSteelRegions()

    # convert to magpie object
    x <- as.magpie(x, spatial = "country_name")

    x <- x * 1e3 # convert from kt to tonnes

    return(x)
  }

  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "production" = function() {
      x <- .readCommonSourceFormat("P01_crude_2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "bofProduction" = function() {
      x <- .readCommonSourceFormat("P05_bof_2023-10-23.xlsx")
      return(x)
    },
    "eafProduction" = function() {
      x <- .readCommonSourceFormat("P06_eaf_2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2002, 2005), ] <- x["YUG", seq(2002, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "imports" = function() {
      x <- .readCommonSourceFormat("T02_imports_finished-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "exports" = function() {
      x <- .readCommonSourceFormat("T01_exports_finished-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "scrapImports" = function() {
      x <- .readCommonSourceFormat("T18_imports_scrap-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "scrapExports" = function() {
      x <- .readCommonSourceFormat("T17_exports_scrap-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "indirectImports" = function() {
      x <- .readCommonSourceFormat("I02_indirect_imports_2023-10-23.xlsx")
      return(x)
    },
    "indirectExports" = function() {
      x <- .readCommonSourceFormat("I01_indirect_exports_2023-10-23.xlsx")
      return(x)
    },
    "pigIronProduction" = function() {
      x <- .readCommonSourceFormat("P26_pigiron_2023-10-23.xlsx")
      return(x)
    },
    "pigIronImports" = function() {
      x <- .readCommonSourceFormat("T12_imports_pigiron-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "pigIronExports" = function() {
      x <- .readCommonSourceFormat("T11_exports_pigiron-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "driProduction" = function() {
      x <- .readCommonSourceFormat("P27_driron_2023-10-23.xlsx")
      return(x)
    },
    "driImports" = function() {
      x <- .readCommonSourceFormat("T14_imports_driron-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
    },
    "driExports" = function() {
      x <- .readCommonSourceFormat("T13_exports_driron-2023-10-23.xlsx")

      # fix mislabelled data for 2003-2005 (should be SCG, but is YUG)
      x <- add_columns(x, addnm = "SCG", dim = 1, fill = NA)
      x["SCG", seq(2003, 2005), ] <- x["YUG", seq(2003, 2005), ]
      x <- x["YUG", , , invert = TRUE]

      return(x)
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
    return(switchboard[[subtype]]())
  }
}
