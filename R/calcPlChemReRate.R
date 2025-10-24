#' Calculate Country-Level Chemical Recycling Rate Trajectories
#'
#' Generate time series of chemical recycling share trajectories by sector and region,
#' then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
calcPlChemReRate <- function(target = NULL) {
  # ---------------------------------------------------------------------------
  # Define sectors and regions
  #    - Retrieve manufacturing sectors (excluding 'Total'), optionally filter by 'target'.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping("structuremappingPlasticManu.csv", type = "sectoral", where = "mrmfa")
  all_targets <- setdiff(unique(sector_map$Target), "Total")
  targets <- if (is.null(target)) all_targets else intersect(target, all_targets)
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  regions <- unique(region_map$RegionCode)

  # ---------------------------------------------------------------------------
  # Define time horizon and share bounds
  #    - Years: 1990–2100
  #    - Start share (pre-2020): 0%
  #    - End share (2050 & beyond): 10%
  # ---------------------------------------------------------------------------
  years <- 1990:2100
  bounds <- data.frame(
    Target = targets,
    start  = 0,
    end    = 0.10,
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # Construct full dataset and interpolate
  #    - Expand grid Year × Target × Region
  #    - Merge share bounds and compute piecewise trajectories
  # ---------------------------------------------------------------------------
  traj_df <- expand.grid(
    Year   = years,
    Target = targets,
    Region = regions,
    stringsAsFactors = FALSE
  )
  traj_df <- merge(traj_df, bounds, by = "Target")
  traj_df$value <- with(traj_df, ifelse(
    Year < 2020, start,
    ifelse(
      Year <= 2050,
      start + (Year - 2020)*(end - start)/(2050 - 2020),
      end
    )
  ))
  traj_df <- dplyr::select(traj_df, "Region", "Year", "Target", "value")

  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  # ---------------------------------------------------------------------------
  x <- as.magpie(traj_df, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )

  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1

  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Chemical recycling",
    description = "Chemical recycling rate trajectories aggregated to country level for 1990–2100.",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}


