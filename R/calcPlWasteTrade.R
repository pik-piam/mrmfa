#' Calculate Country-Level Plastic Waste Trade Shares
#'
#' Read UNCTAD regional plastic waste trade data (exports or imports),
#' backcast missing historical years 1990-2004 and fill
#' future years 2024-2100 assuming a linear decline to 0 in 2030,
#' then aggregate to country level.
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
  # Load regional plastic waste trade data and backcast to 1950 using consumption data
  # ---------------------------------------------------------------------------
  if (subtype == "export") {
    datatype = "Exports"
  } else if (subtype == "import") {
    datatype = "Imports"
  } else {
    stop("Invalid subtype. Choose 'export' or 'import'.")
  }

  trade <- calcOutput("PlUNCTAD", subtype = "Waste_Region")
  trade_filtered <- collapseNames(trade[, , getNames(trade, dim=1)==datatype])

  consumption <- collapseNames(dimSums(calcOutput("PlConsumptionByGood"), dim = 3))
  hist_df <- toolBackcastByReference2D(trade_filtered, consumption) %>%
    as.data.frame() %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year)))%>%
    dplyr::select(-"Cell",-"Data1")

  # ---------------------------------------------------------------------------
  # Fill future years (2024-2100) with 2023 values
  # ---------------------------------------------------------------------------
  base_2023 <- hist_df %>% dplyr::filter(.data$Year == 2023) %>% dplyr::select(-"Year")
  future_years <- 2024:2100
  future_df <- tidyr::expand_grid(
    Region = unique(hist_df$Region),
    Year   = future_years
  ) %>%
    # attach the 2023 baseline values for each Region
    dplyr::left_join(base_2023, by = c("Region")) %>%
    # scale factor: linear decline from 1 (at 2022) to 0 at 2030; 0 afterwards
    dplyr::mutate(
      .scale = dplyr::case_when(
        Year <= 2030 ~ (2030 - Year) / (2030 - 2023),  # 2024→6/7, …, 2030→0
        TRUE ~ 0
      )
    ) %>%
    # multiply all numeric value columns by .scale (leave keys intact)
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric) & !dplyr::any_of(c("Year", ".scale")),
        .fns  = ~ .x * .scale
      )
    ) %>%
    dplyr::select(-.scale)              # drop helper column
  full_df <- dplyr::bind_rows(hist_df, future_df) %>%
    dplyr::arrange(.data$Region, .data$Year)

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level
  # ---------------------------------------------------------------------------
  x <- as.magpie(full_df %>% dplyr::select("Region", "Year", "Value"), spatial = 1, temporal = 2)
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
    description = "Country-level plastic waste trade flow aggregated for 1990-2100.",
    note        = "dimensions: (Time,Region,value)"
  ))
}
