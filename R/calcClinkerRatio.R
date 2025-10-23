#' Calculates global clinker ratio by employing cement and clinker production from Andrew's 2019 paper.
#'
#' @author Bennet Weiss
calcClinkerRatio <- function() {
  ratio_GNR <- readSource("GNR", subtype = "clinker_ratio")
  # Production
  prod_cement <- calcOutput("BinderProduction", subtype = "cement", aggregate = FALSE)
  prod_clinker <- calcOutput("BinderProduction", subtype = "clinker", aggregate = FALSE)

  # Trade
  trade_clinker <- calcOutput("MaterialTrade", subtype = "clinker", aggregate = FALSE)

  consum_clinker <- prod_clinker
  consum_clinker[, getYears(trade_clinker), ] <- consum_clinker[, getYears(trade_clinker), ] - trade_clinker

  # initiate clinker ratio by "clinker use" / "cement production"
  ratio <- new.magpie(
    cells_and_regions = getItems(prod_cement, dim = 1),
    years = getYears(prod_cement)
  )
  ratio[, getYears(consum_clinker), ] <- consum_clinker / prod_cement[, getYears(consum_clinker), ]

  # restrict clinker ratio to realistic values
  ratio[ratio < 0.6 | ratio > 0.99] <- NA
  ratio <- replace_non_finite(ratio, NA)

  # replace data with GNR values where not at least n_NA values are available
  # Andrew (2019) used GNR values where no other data was available
  country_mask <- toolMaskNACountries(ratio[, getYears(ratio_GNR)], n_noNA = 5)
  ratio[country_mask, ] <- NA
  ratio[, getYears(ratio_GNR)][country_mask, ] <- ratio_GNR[country_mask, ]

  # replace data before 1970 with 0.95 (as Andrew 2019)
  ratio[, getYears(ratio, as.integer = TRUE) <= 1970] <- 0.95

  # Linearly extrapolate till available data (as Andrew 2019)
  # Also extrapolate any other missing data
  all_regions <- getItems(ratio, dim = 1)
  all_years <- getYears(ratio)
  # looping through regions is necessary as each region might have different temporal gaps
  # optimization for country_mask regions could be possible.
  for (i in seq_along(all_regions)) {
    region_ratio <- toolRemoveNA(ratio[all_regions[i], ])
    years_to_interpolate <- all_years[!all_years %in% getYears(region_ratio)]
    ratio[all_regions[i], ] <- time_interpolate(region_ratio,
      years_to_interpolate,
      integrate_interpolated_years = TRUE
    )
  }

  weight <- prod_cement
  # until 1970 clinker ratio is the same everywhere, anyways
  weight[, getYears(weight, as.integer = TRUE) <= 1970] <- 1
  unit <- "ratio"
  description <- paste(
    "Annual clinker-to-cement ratio, calculated similiar as by Andrew (2019).",
    "Calculated by (apparent clinker consumption) / (cement production).",
    "For data gaps, use GNR data. Before 1970, assume constant clinker ratio of 0.95.",
    "Remaining gaps filled by linear extrapolation."
  )
  output <- list(x = ratio, weight = weight, unit = unit, description = description)
  return(output)
}
