#' Calculate Country-Level Plastic Waste Trade Shares
#'
#' Read UNCTAD regional plastic waste trade data (exports or imports),
#' fill missing historical years 1990–2004 with 2005 values and
#' future years 2023-2100 with 2022 values, then aggregate to country level.
#'
#' @param subtype Character; flow to extract:
#'   \itemize{
#'     \item "export"  - Exports of plastic waste
#'     \item "import"  - Imports of plastic waste
#'   }
#'
#' @author Qianzhi Zhang
calcPlWasteTrade <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Load regional plastic waste trade data
  # ---------------------------------------------------------------------------
  region_df <- calcOutput(
    "PlUNCTAD", subtype = "Waste_Region"
  ) %>%
    as.data.frame() %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year))) %>%
    dplyr::select("Region", "Year", "Data1", "Data2", "Value")

  # ---------------------------------------------------------------------------
  # Filter for exports or imports
  # ---------------------------------------------------------------------------
  if (subtype == "export") {
    flow_df <- region_df %>% dplyr::filter(.data$Data1 == "Exports") %>% dplyr::select(-"Data1")
  } else if (subtype == "import") {
    flow_df <- region_df %>% dplyr::filter(.data$Data1 == "Imports") %>% dplyr::select(-"Data1")
  } else {
    stop("Invalid subtype. Choose 'export' or 'import'.")
  }

  # ---------------------------------------------------------------------------
  # Fill missing historical years (1990–2004) with 2005 values
  # ---------------------------------------------------------------------------
  base_2005 <- flow_df %>% dplyr::filter(.data$Year == 2005) %>% dplyr::select(-"Year")
  hist_years <- 1990:2004
  hist_df <- tidyr::expand_grid(
    Region = unique(flow_df$Region),
    Year   = hist_years,
    Data2  = unique(flow_df$Data2)
  ) %>%
    dplyr::left_join(base_2005, by = c("Region", "Data2"))

  # ---------------------------------------------------------------------------
  # Fill future years (2023–2100) with 2022 values
  # ---------------------------------------------------------------------------
  base_2022 <- flow_df %>% dplyr::filter(.data$Year == 2022) %>% dplyr::select(-"Year")
  future_years <- 2023:2100
  future_df <- tidyr::expand_grid(
    Region = unique(flow_df$Region),
    Year   = future_years,
    Data2  = unique(flow_df$Data2)
  ) %>%
    dplyr::left_join(base_2022, by = c("Region", "Data2"))

  # ---------------------------------------------------------------------------
  # Combine core, historical, and future data, and compute share
  # ---------------------------------------------------------------------------
  core_df <- flow_df %>%
    dplyr::filter(!(.data$Year %in% c(hist_years, future_years))) %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year)))

  full_df <- dplyr::bind_rows(core_df, hist_df, future_df) %>%
    dplyr::mutate(Year = as.integer(.data$Year)) %>%
    dplyr::arrange(.data$Region, .data$Year)

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level
  # ---------------------------------------------------------------------------
  x <- as.magpie(full_df %>% dplyr::select("Region", "Year", "Data2", "Value"), spatial = 1, temporal = 2)
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")

  gdp_ssp2 <- calcOutput("GDP", scenario="SSP2", average2020 = FALSE, naming = "scenario", aggregate = FALSE)[,"y2019", "SSP2"]
  x <- toolAggregate(x, rel = region_map, dim = 1, from = "RegionCode", to = "CountryCode", weight = gdp_ssp2[unique(region_map$CountryCode), , ])
  getNames(x) <- NULL

  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  return(list(
    x           = x,
    weight      = NULL,
    unit        = "Mt Plastic",
    description = "Country-level plastic waste trade flow aggregated for 1990–2100.",
    note        = "dimensions: (Time,Region,value)"
  ))
}
