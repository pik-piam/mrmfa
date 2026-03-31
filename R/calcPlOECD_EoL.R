#' Calculate Country-Level End-of-Life Plastic Fate Shares
#'
#' Compute end-of-life fate ratios of plastics by country,
#' based on OECD regional waste EOL data (1990–2019).
#'
#' @author Qianzhi Zhang
#'
#'
#'

# TODO: currently used nowhere, can this function be deleted?
calcPlOECD_EoL <- function() {
  # ---------------------------------------------------------------------------
  # Load and clean regional EoL data (1990–2019)
  #    - Read OECD waste end-of-life outputs by region.
  #    - Exclude totals and not applicable categories.
  # ---------------------------------------------------------------------------
  eps <- 1e-9
  eol_df <- calcOutput(
    "PlOECD",
    subtype = "WasteEOL_1990-2019_region", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::filter(!.data$Data1 %in% c("Total", "Not applicable")) %>%
    dplyr::select(-"Cell", -"Data2")

  # ---------------------------------------------------------------------------
  # Calculate per-region fate ratios
  #    - Sum values per region-year and compute ratio for each fate category.
  # ---------------------------------------------------------------------------
  eol_df <- eol_df %>%
    dplyr::group_by(.data$Region, .data$Year) %>%
    dplyr::mutate(
      total = sum(.data$Value, na.rm = TRUE),
      ratio = .data$Value / (.data$total + eps)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"total", -"Value") %>%
    dplyr::rename(EoL_Ratio = "ratio")

  x <- as.magpie(eol_df, spatial = 1, temporal = 2)

  return(list(
    x            = x,
    isocountries = FALSE,
    unit         = "%",
    description  = "End-of-life fate ratios of plastic aggregated to country level."
  ))
}
