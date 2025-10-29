#' Calculate Country-Level Bio-based Plastic Share Trajectories
#'
#' Generate time series of bio-based plastic share trajectories by sector
#' and aggregate from regions to countries for 1990-2100.
#'
#' @author Qianzhi Zhang
#'
calcPlBioRate <- function() {
  # ---------------------------------------------------------------------------
  # Load sectoral mapping and define sectors
  #    - Retrieve sectoral targets from manufacturing mapping, excluding totals.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping(
    "structuremappingPlasticManu.csv", type = "sectoral", where = "mrmfa"
  )
  targets <- unique(sector_map$Target)
  targets <- setdiff(targets, "Total")

  # ---------------------------------------------------------------------------
  # Load regional mapping and define regions
  #    - Retrieve regional codes from mapping.
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mappingfolder"
  )
  regions <- unique(region_map$RegionCode)

  # ---------------------------------------------------------------------------
  # Define time horizon and bio-based share bounds
  #    - Years: 1990-2100
  #    - Starting share (2020): 0
  #    - End share (2050): 20%
  # ---------------------------------------------------------------------------
  years <- 1990:2100
  share_bounds <- data.frame(
    Target = targets,
    start  = 0,
    end    = 0.2
  )

  # ---------------------------------------------------------------------------
  # Build full data frame of trajectories
  #    - Expand grid of Year x Sector x Region
  #    - Merge with share bounds and interpolate linearly
  # ---------------------------------------------------------------------------
  traj_df <- expand.grid(
    Year   = years,
    Target = targets,
    Region = regions,
    stringsAsFactors = FALSE
  )
  traj_df <- merge(traj_df, share_bounds, by = "Target")
  traj_df$value <- with(traj_df, ifelse(
    Year <= 2020, start,
    ifelse(
      Year <= 2050,
      start + (Year - 2020) * (end - start) / (2050 - 2020),
      end
    )
  ))
  traj_df <- dplyr::select(traj_df, "Region", "Year", "Target", "value")

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level
  #    - Map regional trajectories to countries (equal weights)
  # ---------------------------------------------------------------------------
  x <- as.magpie(traj_df, spatial = 1, temporal = 2)
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
    unit        = "% Bio-based Plastic",
    description = "Projected bio-based plastic share by sector, aggregated to country level for 1990-2100.",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}

