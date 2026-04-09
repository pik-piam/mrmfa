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

  regional_df <- plasticOutlook %>%
    as.data.frame() %>%
    dplyr::filter(.data$Data1 != "Total", .data$Data2 != "Total") %>%
    dplyr::group_by(.data$Region, .data$Year, .data$Data2) %>%
    dplyr::summarise(Value_sum = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(.data$Region, .data$Year) %>%
    dplyr::mutate(
      "share" = .data$Value_sum / sum(.data$Value_sum, na.rm = TRUE),
      "share" = ifelse(is.nan(.data$share), 0, .data$share)
      ) %>%
    dplyr::ungroup()

  x <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "share")],
    spatial = 1, temporal = 2
  )

  weight <- x
  weight[, , ] <- plasticOutlook[, , "Total.Total"]

  return(list(
    x = x,
    weight = weight,
    unit = "%",
    description = "Sectoral plastic use shares aggregated to country level for 2019."
  ))
}
