#' Calculate Country-Level Plastics Trade for Various Categories
#'
#' Reads UNCTAD plastics trade (exports or imports) data at regional level,
#' fills 1990-2004 years with data of 2005, and aggregates to country level.
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
  # Load regional trade data for the selected category
  # ---------------------------------------------------------------------------
  region_df <- calcOutput(
    "PlUNCTAD", subtype = subtype_region
  ) %>%
    as.data.frame()

  # ---------------------------------------------------------------------------
  # Filter by export or import flows
  # ---------------------------------------------------------------------------
  flow_df <- region_df %>%
    dplyr::filter(.data$Data1 == flow_label)  %>%
    dplyr::select("Region","Year","Value")

  # ---------------------------------------------------------------------------
  # Fill missing historical years (1990-2004) using 2005 values
  # ---------------------------------------------------------------------------
  base_2005 <- flow_df %>%
    dplyr::filter(.data$Year == 2005) %>%
    dplyr::select(-"Year")
  hist_years <- 1990:2004
  hist_df <- expand.grid(
    Region = unique(flow_df$Region),
    Year   = hist_years,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(base_2005, by = c("Region"))

  # ---------------------------------------------------------------------------
  # Combine original, and historical data, then sort by year
  # ---------------------------------------------------------------------------
  core_df <- flow_df %>%
    dplyr::filter(!.data$Year %in% hist_years) %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year)))
  full_df <- dplyr::bind_rows(core_df, hist_df) %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year))) %>%
    dplyr::arrange(.data$Year)

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level using GDP weights
  # ---------------------------------------------------------------------------
  x <- as.magpie(
    full_df %>% dplyr::select("Region", "Year", "Value"),
    spatial = 1, temporal = 2
  )
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mappingfolder"
  )
  gdp_ssp2 <- calcOutput(
    "GDP", scenario="SSP2", average2020 = FALSE, naming = "scenario", aggregate = FALSE
  )[, paste0("y", 1990:2022), "SSP2"]
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
      "Country-level %s plastics %s (1990-2100)", category, flow_label
    ),
    note        = "dimensions: (Historic Time,Region,value)"
  )
}
