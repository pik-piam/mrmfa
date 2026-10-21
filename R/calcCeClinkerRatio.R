#' Calculates global clinker ratio by employing cement and clinker production from Andrew's 2019 paper.
#'
#' @author Bennet Weiss
calcCeClinkerRatio <- function() {
  ratio_GNR <- readSource("GNR", subtype = "clinker_ratio")
  # Production
  prod_cement <- calcOutput("CeCementProduction", aggregate = FALSE)
  prod_clinker <- calcOutput("CeClinkerProduction", aggregate = FALSE, warnNA = FALSE)

  # Trade
  # Note that the trade is not balanced, significant especially pre-1995
  clinker_imports <- calcOutput("CeTrade",
    category = "clinker",
    subtype = "Imports",
    regionmapping = "ISO_2_ISO.csv"
  )
  clinker_exports <- calcOutput("CeTrade",
    category = "clinker",
    subtype = "Exports",
    regionmapping = "ISO_2_ISO.csv"
  )
  trade <- toolBalanceTrade(clinker_imports, clinker_exports, to = "imports")
  clinker_imports <- trade$imports
  clinker_exports <- trade$exports

  # Consumption
  clinker_years <- getYears(prod_clinker)
  consum_clinker <- (
    prod_clinker
    + clinker_imports[, clinker_years]
    - clinker_exports[, clinker_years]
  )

  # initiate clinker to cement ratio by "clinker consumption" / "cement production"
  ratio <- new.magpie(
    cells_and_regions = getItems(prod_cement, dim = 1),
    years = getYears(prod_cement)
  )
  ratio[, clinker_years, ] <- consum_clinker / prod_cement[, clinker_years, ]

  # only use years with observed (not forecasted) clinker trade
  baci <- quitte::madrat_mule(readSource("BACI", subtype = "cement-clinker", subset = "92"))
  last_trade_year <- toolLastCompleteBACIYear(baci$t)
  ratio[, getYears(ratio, as.integer = TRUE) > last_trade_year] <- NA

  # restrict clinker ratio to realistic values (0.5-0.97).
  ratio[ratio < 0.5 | ratio > 1.1] <- NA
  ratio <- replace_non_finite(ratio, NA)
  ratio[ratio > 0.97] <- 0.97 # max GNR value is 0.967

  # replace data with GNR values where not at least n_valid values are available.
  # Andrew (2019) used GNR values where no other data was available.
  country_mask <- toolMaskNACountries(ratio[, getYears(ratio_GNR)], n_valid = 10)
  ratio[country_mask, ] <- NA
  ratio[, getYears(ratio_GNR)][country_mask, ] <- ratio_GNR[country_mask, ]

  # replace data before 1970 with 0.95 (as Andrew 2019)
  ratio[, getYears(ratio, as.integer = TRUE) <= 1970] <- 0.95

  # Linearly interpolate till available data (as Andrew 2019)
  # Also interpolate any other missing data, last valid value is held constant
  ratio <- toolInterpolate(ratio, type = "linear", extrapolate = TRUE)

  weight <- prod_cement
  weight[weight == 0] <- 1e-9
  unit <- "ratio"
  description <- paste(
    "Annual clinker-to-cement ratio, calculated similar as by Andrew (2019).",
    "Calculated by (apparent clinker consumption) / (cement production)",
    "for years with observed clinker trade. Values below 0.5 or above 1.1 are removed,",
    "values between 0.97 and 1.1 are set to 0.97.",
    "For data gaps, use GNR data. Before 1970, assume constant clinker ratio of 0.95.",
    "Remaining gaps filled by linear interpolation, the last valid value is held constant."
  )
  note <- "dimensions: (Historical Time,Region,value)"
  output <- list(x = ratio, weight = weight, unit = unit, description = description, note = note)
  return(output)
}
