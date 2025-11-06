#' Calculate Plastic Use Share by Country
#'
#' Compute the share of plastic use by sector for each country in 2019,
#' based on regional OECD data and supplemental EU shares.
#'
#' @author Qianzhi Zhang
#'
calcPlGoodShare <- function() {
  # ---------------------------------------------------------------------------
  # Calculate share of plastic use by sector (plastics application) from OECD data
  # - Read OECD plastic use outputs at regional level.
  # - Exclude total categories and compute sectoral sums (summarise over all polymers) and shares.
  # ---------------------------------------------------------------------------
  regional_df <- calcOutput(
    "PlOECD",
    subtype = "Use_2019_region", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::filter(.data$Data1 != "Total", .data$Data2 != "Total") %>%
    dplyr::group_by(.data$Region, .data$Year, .data$Data2) %>%
    dplyr::summarise(Value_sum = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$Region, .data$Year) %>%
    dplyr::mutate(share = .data$Value_sum / sum(.data$Value_sum, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # ---------------------------------------------------------------------------
  # Replace shares for EU with EU plastic share reference data
  # - Read PlasticEurope shares
  # - Map sectors of EU shares to sectors of OECD shares, for mapping "Agriculture" and "Others" to "Others" and "Textiles" use weights from OECD data
  # ---------------------------------------------------------------------------
  eu_share <- readSource("PlasticsEurope", subtype = "PlasticShare_EU", convert = FALSE)

  sector_map <- toolGetMapping(
    "structuremappingPlasticShare.csv",
    type = "sectoral", where = "mrmfa"
  )
  weights_eu <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "Value_sum")],
    spatial = 1, temporal = 2
  )
  eu_share_agg <- toolAggregate(
    eu_share / 100,
    rel = sector_map, dim = 3,
    weight = weights_eu["EUR", "y2019", ],
    from = "Source", to = "Target"
  )

  regional_share <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "share")],
    spatial = 1, temporal = 2
  )
  regional_share["EUR", "y2019", ] <- eu_share_agg["EUR", "y2019", ]

  # ---------------------------------------------------------------------------
  # Aggregate shares to country level
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv",
    type = "regional", where = "mappingfolder"
  )
  country_share <- toolAggregate(
    regional_share,
    rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )

  # ---------------------------------------------------------------------------
  # Prepare final weight object and return
  #    - Set all aggregation weights to 1.
  # ---------------------------------------------------------------------------
  weight <- country_share
  weight[, ] <- 1

  return(list(
    x = country_share,
    weight = weight,
    unit = "%",
    description = "Sectoral plastic use shares aggregated to country level for 2019."
  ))
}
