#' Calculate Country-Level Mechanical Recycling Yield Trajectories
#'
#' Generate time series of mechanical recycling yield (efficiency) trajectories by sector and region,
#' then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
calcPlMechReYield <- function() {
  # ---------------------------------------------------------------------------
  # Define sectors and regions
  #    - Retrieve manufacturing sectors (excluding 'Total') and regional codes.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping(
    "structuremappingPlasticManu.csv", type = "sectoral", where = "mrmfa"
  )
  targets <- setdiff(unique(sector_map$Target), "Total")
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mappingfolder"
  )
  regions <- unique(region_map$RegionCode)

  # ---------------------------------------------------------------------------
  # Define time horizon and yield bounds
  #    - Years: 1990–2100
  #    - Start and end yield fixed at 79% for all sectors.
  #      Source: weighted average (based on waste generation) of yields for different polymers, see POSTED
  # ---------------------------------------------------------------------------
  years <- 1990:2100
  bounds <- data.frame(
    Target = targets,
    start  = 0.79,
    end    = 0.79,
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # Construct full dataset and compute yield trajectories
  #    - Expand grid Year × Target × Region
  #    - Merge bounds and linearly interpolate beyond 2020 to 2100
  # ---------------------------------------------------------------------------
  traj_df <- expand.grid(
    Year   = years,
    Target = targets,
    Region = regions,
    stringsAsFactors = FALSE
  )
  traj_df <- merge(traj_df, bounds, by = "Target")
  traj_df$value <- with(traj_df, ifelse(
    Year <= 2020,
    start,
    start + (Year - 2020) * (end - start) / (2100 - 2020)
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
    unit        = "% Mechanical recycling efficiency",
    description = "Mechanical recycling yield trajectories aggregated to country level for 1990–2100.",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}


