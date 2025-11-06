#' Read IEDC data
#' @description
#'
#' Read data from the Industrial Ecology Data Commons (IEDC). So far
#' only data by Pauliuk et al. (2019) on pig iron is used.
#'
#' @param subtype Options: 'pigIronProduction', 'pigIronImports', 'pigIronExports'
#'
#' @author Merlin Jo Hosak
#' @export
readIEDC <- function(subtype = "pigIronProduction") {
  version <- "v1.0"
  switchboard <- list(
    "pigIronProduction" = function() {
      x <- readNormalIEDC("1_F_steel_200r_F_1_2_pig_iron_production.xlsx", version)
      return(x)
    },
    "pigIronImports" = function() {
      x <- readNormalIEDC("1_F_steel_200R_F_21_2_pig_iron_import.xlsx", version,
        region_col = "aspect 6 : destination_region"
      )
      return(x)
    },
    "pigIronExports" = function() {
      x <- readNormalIEDC("1_F_steel_200R_F_2_30_pig_iron_export.xlsx", version)
    },
    NULL
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

readNormalIEDC <- function(filename, version, region_col = "aspect 4 : origin_region") {
  path <- paste0(version, "/", filename)
  x <- read_excel(path, sheet = "Data") |>
    select(
      time = `aspect 7 : time`,
      region = all_of(region_col),
      value
    )

  # Pivot wider: one column per year
  x_wide <- x |>
    pivot_wider(names_from = time, values_from = value)

  # Get sorted column names (excluding the first id col)
  year_cols <- sort(as.numeric(names(x_wide)[-1]))

  # Select by *column names* (as character)
  x_wide <- x_wide |>
    select(region, all_of(as.character(year_cols)))

  # Convert to magpie
  # region = 1st column, years = column names
  x <- as.magpie(x_wide, spatial = 1)

  countries <- getItems(x, dim = 1)
  countries <- gsub("_", ".", countries) # replace underscores with dots as magclass sometimes does the opposite

  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrindustry"))$IgnoredRegions
  getItems(x, dim = 1) <- toolCountry2isocode(countries, ignoreCountries = ignore)

  # remove new rows with NA in country_name column (that were ignored)
  x <- x[!is.na(getItems(x, dim = 1)), ] # TODO standardize!?

  x <- x * 1e3 # convert from Gigagrams to tons...

  return(x)
}
