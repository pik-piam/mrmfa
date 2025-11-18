#' Read IEDC data
#' @description
#'
#' Read data from the Industrial Ecology Data Commons (IEDC). So far
#' only data by Pauliuk et al. (2019) on pig iron is used.
#'
#' @param subtype Options: 'pigIronProduction', 'pigIronImports', 'pigIronExports'
#'
#' @author Merlin Jo Hosak
readIEDC <- function(subtype = "pigIronProduction") {
  version <- "v1.0"

  .readCommonSourceFormat <- function(filename, version, regionCol) {
    path <- file.path(".", version, filename)
    x <- read_excel(path, sheet = "Data") %>%
      select(
        "time" = "aspect 7 : time",
        "country_name" = regionCol,
        "value" = "value"
      ) %>%
      mutate("time" = as.numeric(.data$time))

    x <- as.magpie(x, spatial = 2)

    # convert from Gigagrams to tons
    x <- x * 1e3

    return(x)
  }

  switchboard <- list(
    "pigIronProduction" = function() {
      x <- .readCommonSourceFormat(
        "1_F_steel_200R_F_1_2_pig_iron_production.xlsx", version,
        regionCol = "aspect 4 : origin_region"
      )
      return(x)
    },
    "pigIronImports" = function() {
      x <- .readCommonSourceFormat(
        "1_F_steel_200R_F_21_2_pig_iron_import.xlsx", version,
        regionCol = "aspect 6 : destination_region"
      )
      return(x)
    },
    "pigIronExports" = function() {
      x <- .readCommonSourceFormat(
        "1_F_steel_200R_F_2_30_pig_iron_export.xlsx", version,
        regionCol = "aspect 4 : origin_region"
      )
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
