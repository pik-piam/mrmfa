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

  plasticOutlook <- calcOutput("PlOECD", subtype = "Use_2019_region", aggregate = FALSE)

  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")

  # we always aggregate to H12 (independently of the set region mapping), do the share
  # calculation on H12 regions and then carry over the shares to the countries according to
  # H12 regions
  # FIXME: consider doing the shares calculation directly on country level, as there will be
  # a mismatch when using other regions and H12 also has some questionable regions like
  # CAZ, grouping New Zealand and Australia with Canada


  plasticOutlook <- toolAggregate(plasticOutlook, region_map,
    from = "CountryCode", to = "RegionCode", dim = 1
  )

  regional_df <- plasticOutlook %>%
    as.data.frame() %>%
    dplyr::filter(.data$Data1 != "Total", .data$Data2 != "Total") %>%
    dplyr::group_by(.data$Region, .data$Year, .data$Data2) %>%
    dplyr::summarise(Value_sum = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$Region, .data$Year) %>%
    dplyr::mutate(share = .data$Value_sum / sum(.data$Value_sum, na.rm = TRUE)) %>%
    dplyr::ungroup()

  regional_share <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "share")],
    spatial = 1, temporal = 2
  )

  # ---------------------------------------------------------------------------
  # Aggregate shares to country level
  # ---------------------------------------------------------------------------

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
  weight[, , ] <- 1

  return(list(
    x = country_share,
    weight = weight,
    unit = "%",
    description = "Sectoral plastic use shares aggregated to country level for 2019."
  ))
}
