#' Split World Steel trade into bilateral trade by using proxy bilateral BACI trade data
#' and aggregate via a custom aggregation function to filter out intra-regional trade
#' (since we don't have a list with all HS codes and corresponding steel shares,
#' BACI trade data cannot be used directly, since it does not cover the full scope)
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
#' @param include_intra_regional bool if intra-regional trade should be included
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
calcCeTrade <- function(subtype, category, HS = "92", include_intra_regional = FALSE) {

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
  # Note: reference has region-specific units and can therefore not be aggregated.
  years <- 1900:2021

  # cement production data as base reference
  production_reference <- calcOutput("CeBinderProduction", subtype = "cement", aggregate = FALSE, years = years)
  reference <- production_reference

  # complement base reference with GDP data where in regions without production
  gdp_reference <- calcOutput("CoGDP1900To2150", aggregate = FALSE, years = years)
  zero_prod_regions <- getRegions(production_reference[dimSums(production_reference, dim = c(2, 3)) == 0, , ])
  reference[zero_prod_regions, ] <- gdp_reference[zero_prod_regions, ]

  # add global shipping cost to base reference as bulk trade generally got cheaper over time
  shipping_cost <- readSource("OWID", subtype = "shipping_costs")
  shipping_cost <- toolInterpolate(shipping_cost, years = years, extrapolate = TRUE)
  shipping_cost <- toolTimeSpline(shipping_cost, dof = 10)
  reference <- reference / shipping_cost

  # replace US reference with actual trade data from USGS
  us_reference <- readSource("USGSDS140", subtype = subtype)["USA",]
  reference["USA", ] <- us_reference

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

    # Note that if reference has more recent data than x, x will be forecasted, too.
    x <- toolBackcastByReference(x, ref)

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
