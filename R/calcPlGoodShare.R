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
