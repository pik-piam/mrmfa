#' Calculate Country-Level Plastics Trade for Various Categories
#'
#' Reads UNCTAD plastics trade (exports or imports) data at regional level,
#' backcasts data to 1950 to fill missing years 1950-2004 years,
#' and aggregates to country level.
#'
#' @param category Character; product category:
#'   \itemize{
#'     \item "final"        - Final plastics
#'     \item "primary"      - Primary plastics
#'     \item "intermediate" - Intermediate forms of plastic
#'     \item "manufactured" - Intermediate manufactured plastic goods
#'   }
#' @param flow_label Character; trade flow:
#'   \itemize{
#'     \item "Exports" - Exports
#'     \item "Imports" - Imports
#'   }
#' @author Qianzhi Zhang
calcPlTrade <- function(
    category = c("final", "primary", "intermediate", "manufactured"),
    flow_label     = c("Exports", "Imports")
) {
  # ---------------------------------------------------------------------------
  # Match inputs and map to UNCTAD subtype identifier
  # ---------------------------------------------------------------------------
  category <- match.arg(category)
  flow_label <- match.arg(flow_label)
  subtype_map <- list(
    final        = "Final_Region",
    primary      = "Primary_Region",
    intermediate = "Intermediate_Region",
    manufactured = "Manufactured_Region"
  )
  subtype_region <- subtype_map[[category]]

  # ---------------------------------------------------------------------------
  # Load regional trade data for the selected category and backcast to 1950
  # ---------------------------------------------------------------------------
  trade <- calcOutput("PlUNCTAD", subtype = subtype_region)
  trade_filtered <- collapseNames(trade[, , getNames(trade, dim=1)==flow_label])

  consumption <- collapseNames(dimSums(calcOutput("PlConsumptionByGood"), dim = 3))
  hist_df <- toolBackcastByReference2D(trade_filtered, consumption) %>%
    as.data.frame() %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year)))%>%
    dplyr::select(-"Cell",-"Data1")

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level using GDP weights
  # ---------------------------------------------------------------------------
  x <- as.magpie(
    hist_df %>% dplyr::select("Region", "Year", "Value"),
    spatial = 1, temporal = 2
  )
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mappingfolder"
  )
  gdp_ssp2 <- calcOutput(
    "GDP", scenario="SSP2", average2020 = FALSE, naming = "scenario", aggregate = FALSE
  )[, paste0("y", 1960:2023), "SSP2"]
  x <- toolAggregate(
    x,
    rel    = region_map,
    dim    = 1,
    from   = "RegionCode",
    to     = "CountryCode",
    weight = gdp_ssp2[unique(region_map$CountryCode), , ]
  )
  getNames(x) <- NULL

  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  list(
    x           = x,
    weight      = NULL,
    unit        = "Mt Plastic",
    description = sprintf(
      "Country-level %s plastics %s (1990-2023)", category, flow_label
    ),
    note        = "dimensions: (Historic Time,Region,value)"
  )
}
