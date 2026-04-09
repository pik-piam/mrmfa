#' Calculate Country-Level Material Good Shares
#'
#' Compute material share of different goods from 2019 OECD use data
#'
#' @author Qianzhi Zhang
#'
calcPlOECD_MGshare <- function() {
  # ---------------------------------------------------------------------------
  # Load and filter regional use data (2019)
  #    - Read regional use output, exclude 'Total' categories.
  # ---------------------------------------------------------------------------

  plOECD <- calcOutput("PlOECD", subtype = "Use_2019_region", aggregate = FALSE)

  use_df <- plOECD %>%
    as.data.frame() %>%
    dplyr::select(-"Cell") %>%
    dplyr::filter(.data$Data1 != "Total", .data$Data2 != "Total")

  # ---------------------------------------------------------------------------
  # Compute material shares by good
  #    - Group by region, year, and good (Data2), then calculate share of each subcategory (Data1).
  # ---------------------------------------------------------------------------
  ratio_df <- use_df %>%
    dplyr::group_by(.data$Region, .data$Year, .data$Data2) %>%
    dplyr::mutate(
      "total_by_good" = sum(.data$Value, na.rm = TRUE),
      "MaterialShare" = .data$Value / .data$total_by_good,
      "MaterialShare" = ifelse(is.nan(.data$MaterialShare), 0, .data$MaterialShare)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("Region", "Data2", "Data1", "MaterialShare")

  x <- as.magpie(ratio_df, spatial = 1)

  weight <- x
  total <- plOECD %>%
    mselect("Plastic polymer" = "Total", collapseNames = TRUE)
  weight[, , ] <- total[, , getNames(weight, dim=1)]

  return(list(
    x           = x,
    weight      = weight,
    unit        = "fraction",
    description = "Material share of plastics in different goods aggregated to regional level for 2019.",
    note        = "dimensions: (Region,Good,Material,value)"
  ))
}
