#'Calculate Country-Level Plastic Collection Rate Trajectories
#'
#' Build time series of plastic collection rates by sector and region,
#' apply fixed and interpolated values, then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#' @importFrom tidyr crossing
#' @importFrom stats approx
#'
calcPlCollRate <- function() {
  # ---------------------------------------------------------------------------
  # Load and clean regional EoL data
  #    - Read end-of-life outputs and exclude non-collection categories.
  # ---------------------------------------------------------------------------
  eol_df <- calcOutput(
    "PlOECD_EoL", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::select(-"Cell") %>%
    dplyr::filter(!.data$Data1 %in% c("Littered", "Mismanaged")) %>%
    dplyr::mutate(
      Year = as.integer(as.character(.data$Year))
    ) %>%
    dplyr::group_by(.data$Region, .data$Year) %>%
    dplyr::summarise(
      collected = sum(.data$Value, na.rm = TRUE),
      .groups = "drop"
    )

  # ---------------------------------------------------------------------------
  # Apply fixed collection rates for China
  #    - Use reported rates for specific years and interpolate.
  # ---------------------------------------------------------------------------
  # source: Assessment of Plastic Stocks and Flows in China: 1978-2017; 1-(Untreatment share)
  fixed_years <- c(1990, 2005, 2010, 2015, 2017)
  fixed_vals  <- c(0.65, 0.68, 0.84, 0.96, 0.98)

  # Interpolate for China across full timeline
  china_idx <- eol_df$Region == "CHA"
  interp_china <- approx(
    x = fixed_years, y = fixed_vals,
    xout = eol_df$Year[china_idx], rule = 2
  )$y
  eol_df$collected[china_idx] <- interp_china

  # ---------------------------------------------------------------------------
  # Fill 1990–2000 for other regions with 2000 level
  #    - For non-CHA regions, assign 2000 value to 1990–2000 period.
  # ---------------------------------------------------------------------------
  non_cha <- dplyr::filter(eol_df, .data$Region != "CHA")
  value2000 <- non_cha %>%
    dplyr::filter(.data$Year == 2000) %>%
    dplyr::select("Region", val2000 = "collected")

  eol_df <- eol_df %>%
    dplyr::left_join(value2000, by = "Region") %>%
    dplyr::mutate(
      collected = if_else(
        .data$Region != "CHA" & .data$Year >= 1990 & .data$Year <= 2000,
        .data$val2000, .data$collected
      )
    ) %>%
    dplyr::select(-"val2000")

  # ---------------------------------------------------------------------------
  # Extend series to 2100 with linear growth to 100%
  #    - Duplicate 2019 as 2020, then interpolate to reach 1.00 by 2100.
  # ---------------------------------------------------------------------------
  base2019 <- eol_df %>% dplyr::filter(.data$Year == 2019)
  ext_df <- dplyr::bind_rows(
    eol_df,
    dplyr::mutate(base2019, Year = 2020)
  )
  target_final <- 1.0
  future_df <- expand.grid(
    Region = unique(ext_df$Region),
    Year   = 2021:2100,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      dplyr::filter(ext_df, .data$Year == 2020) %>%
        dplyr::select("Region", start = "collected"),
      by = "Region"
    ) %>%
    dplyr::mutate(
      collected = .data$start +
        (.data$Year - 2020) * (target_final - .data$start) / (2100 - 2020)
    ) %>%
    dplyr::select("Region", "Year", "collected")

  final_df <- dplyr::bind_rows(
    dplyr::filter(ext_df, .data$Year <= 2020) %>%
      dplyr::select("Region", "Year", "collected"),
    future_df
  )

  # ---------------------------------------------------------------------------
  # Expand df by material
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping("structuremappingPlasticManu.csv", type = "sectoral", where = "mrmfa")
  targets    <- setdiff(unique(sector_map$Target), "Total")
  exp_df <- crossing(final_df, targets) %>%
    dplyr::select("Region", "Year", "targets", "collected")

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  #    - Map regions to countries with equal weights.
  # ---------------------------------------------------------------------------
  x <- as.magpie(exp_df, spatial = 1, temporal = 2)
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mappingfolder"
  )
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )

  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  #    - Equal weights (1) for all entries
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1

  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Collection rate",
    description = "Plastic collection rate trajectories aggregated to country level for 1990–2100.",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}
