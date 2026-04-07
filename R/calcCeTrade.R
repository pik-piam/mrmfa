#' Calculate cement/clinker trade data (imports and exports).
#' Based on bilateral BACI data (1995-2023).
#' Intra-regional trade is filtered out during aggregation.
#' Extended to 'target_years' by backcasting using
#' cement production, GDP, shipping costs and USGS trade data (for US only) as proxy.
#' Clinker trade is linearly faded out between 1970 and 1950.
#'
#' @param subtype Character string specifying the scope
#'        - Imports
#'        - Exports
#' @param category Character string specifying the stage of trade
#'        - cement
#'        - clinker
#' @param HS Character string specifying the year of the HS (Harmonized System) revision of the data
#'        - 92
#'        - 02
#'        - 17
#'        - 22
#' @param target_years integer vector of target years for the output data.
#' If NULL, all years from reference (cement production) are included.
#' Note: the 'years' argument in calcOutput does not work properly for this function,
#' so 'target years' should be set here instead.
#'
#' @return magpie object of the aggregated trade data
#'
#' @author Bennet Weiss, Leonie Schweiger
#'
#' @seealso [calcOutput()]
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(
#'   type = "CeTrade", subtype = "Imports",
#'   category = "cement", HS = "92"
#' )
#' }
#' @importFrom dplyr select filter rename summarize ungroup
#' @importFrom magclass as.magpie getComment<-
#'
calcCeTrade <- function(subtype, category, HS = "92", include_intra_regional = FALSE, target_years = NULL) {
  # ----------------------------------------------------------------------------
  # Load data
  # ----------------------------------------------------------------------------

  df <- readSource("BACI", subtype = paste("cement", category, sep = "-"), subset = HS) %>%
    quitte::madrat_mule()

  # filter out unknown country codes that are later removed by madrat
  unknown_countries <- c("ZA1", "PUS", "R20")
  df <- df %>% filter(!.data$importer %in% unknown_countries, !.data$exporter %in% unknown_countries)
  df <- df[, c("t", "importer", "exporter", "value")]

  # ----------------------------------------------------------------------------
  # Do what usually happens in convert function
  # ----------------------------------------------------------------------------

  # historical ISO countries SCG and ANT split into SRB & MNE in 2006 and SXM & CUW in 2011, respectively
  # for simplicity, their trades are assigned to their major successor countries SRB and CUW before the split year
  # as they account for >90% of the total plastics trade volume of successor countries
  df$importer[df$importer == "SCG"] <- "SRB"
  df$exporter[df$exporter == "SCG"] <- "SRB"
  df$importer[df$importer == "ANT"] <- "CUW"
  df$exporter[df$exporter == "ANT"] <- "CUW"

  # this mapgie object should actually contain two spatial dimensions "importer" and
  # "exporter", but since madrat does not support regional aggregation for two spatial
  # dimensions with iso countries well (as in trade data), we just use one spatial dimension
  # to comply with the framework and later do the regional aggregation manually
  # via a custom aggregation function
  x <- as.magpie(df, temporal = 1, spatial = 2, datacol = 4)
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  x <- replace_non_finite(x, replace = 0)

  # ----------------------------------------------------------------------------
  # Aggregation to avoid intra-regional trade
  # ----------------------------------------------------------------------------

  # construct reference for backcasting based on production/gdp and shipping costs.
  # Note: reference has region-specific units.

  # cement production data as base reference
  production_reference <- calcOutput("CeBinderProduction", subtype = "cement", aggregate = FALSE, years = target_years)
  reference <- production_reference
  target_years <- getYears(production_reference, as.integer = TRUE)

  # complement base reference with GDP data where in regions without production
  gdp_reference <- calcOutput("CoGDP", aggregate = FALSE, years = target_years)
  zero_prod_regions <- getItems(production_reference[dimSums(production_reference, dim = c(2, 3)) == 0, , ], dim = 1)
  reference[zero_prod_regions, ] <- gdp_reference[zero_prod_regions, ]

  # add global shipping cost to base reference as bulk trade generally got cheaper over time
  shipping_cost <- readSource("OWID", subtype = "shipping_costs")

  last_shipping_cost_year <- getYears(shipping_cost, as.integer = TRUE)[1]
  if (last_shipping_cost_year > target_years[1]) {
    # extend shipping cost: increase linearly before 1930 and keep constant after 2003
    shipping_cost <- toolInterpolate(shipping_cost, years = target_years, extrapolate = TRUE)
    shipping_cost[, target_years < last_shipping_cost_year, ] <- NA
    # increase shipping cost linearly by last shipping cost each 50y
    shipping_cost[, target_years[1], ] <- (
      shipping_cost[, last_shipping_cost_year, ]
      * (1 + (last_shipping_cost_year - target_years[1]) / 50)
    )
    shipping_cost <- toolInterpolate(shipping_cost)
  }
  # smooth shipping cost function
  shipping_cost <- toolTimeSpline(shipping_cost, dof = 10)
  reference <- reference / shipping_cost

  # replace US reference with total clinker + cement trade data from USGS
  us_reference <- readSource("USGSDS140", subtype = subtype)["USA", ]
  us_reference <- toolBackcastByReference(us_reference, production_reference["USA", ], doForecast = FALSE)
  us_reference <- toolBackcastByReference(us_reference, production_reference["USA", ], doForecast = TRUE)
  us_reference <- us_reference[, target_years, ]
  reference["USA", ] <- us_reference

  # fade out clinker trade for the distant past (no clinker trade before 1950)
  if (category == "clinker") {
    # set clinker trade to zero before 1950 and linearly increase to 100% until 1970
    clinker_trade_factor <- new.magpie(cells_and_regions = "GLO", years = target_years, fill = 0)
    clinker_trade_factor <- convergence(
      origin = clinker_trade_factor,
      aim = 1,
      start_year = 1950,
      end_year = 1970,
      type = "linear"
    )
    reference <- reference * clinker_trade_factor
  }

  .customAggregate <- function(x, rel, reference, flow_label) {
    # aggregate to regions filtering out intra-regional trade
    x <- toolAggregateBilateralTrade(x, rel, flow_label)

    # backcast aggregated bilateral trade data to 1900
    ref <- toolAggregate(reference, rel = rel)

    # if some of the regions are missing in x due to the manual aggregation,
    # fill with NA to match all the ref regions
    missingRegions <- setdiff(getItems(ref, dim = 1), getItems(x, dim = 1))
    if (length(missingRegions) > 0) {
      x <- add_columns(x, addnm = missingRegions, dim = 1, fill = NA)
    }

    # Forecasting AND backcasting of x.
    x <- toolBackcastByReference(x, ref, doForecast = FALSE)
    x <- toolBackcastByReference(x, ref, doForecast = TRUE)

    # cut x to target years
    x <- x[, target_years, ]
    return(x)
  }

  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  description <- sprintf(
    "%s %s (1900-2023) based on BACI data (>=1995),
    backcasted using cement production,gdp, shipping costs and USGS trade data.",
    category, subtype
  )

  list(
    x = x,
    weight = NULL,
    unit = "t",
    aggregationFunction = .customAggregate,
    aggregationArguments = list(reference = reference, flow_label = subtype),
    description = description,
    note = "dimensions: (Historic Time,Region,value)"
  )
}
