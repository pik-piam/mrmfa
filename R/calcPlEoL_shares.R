#' Calculate Country-Level End-of-Life Plastic Fate Shares
#'
#' Compute end-of-life fate ratios of plastics by country,
#' based on OECD regional waste EOL data (1990–2019) and
#' country/region-specific datasets (EPA, Plastics Europe and Chinese reports)
#'
#' @author Leonie Schweiger, Qianzhi Zhang
#' @param subtype Character string specifying the EoL treatment
#'        Valid formats include:
#'        - Recycled (share of total collected)
#'        - Landfilled (share of total collected)
#'        - Incinerated (share of total collected)
#'        - Collected (share of total plastic waste)
#'        - All (all shares combined in one magpie object)
#'
#' @importFrom dplyr select filter mutate group_by summarise case_when left_join
#' @importFrom data.table first
#'
calcPlEoL_shares <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Calculate EoL shares from OECD data
  # - OECD data (1990–2019)
  # - data before 2000 all 0, so assume value of year 2000 for 1990-1999
  # - calculate shares
  # ---------------------------------------------------------------------------
  oecd_raw <- calcOutput("PlOECD", subtype = "WasteEOL_1990-2019_region", aggregate=TRUE) %>%
    as.data.frame() %>%
    filter(!.data$Data1 %in% c("Total", "Not applicable")) %>%
    select(-"Cell",-"Data2") %>%
    mutate(Year = as.integer(as.character(.data$Year)))
  oecd <- oecd_raw %>% group_by(.data$Region, .data$Data1) %>%
    mutate(Value = if_else(.data$Year < 2000,
                             first(.data$Value[.data$Year == 2000]),
                             .data$Value),
           Data1 = case_when(.data$Data1 %in% c("Mismanaged","Littered")~"Uncollected", TRUE ~ .data$Data1)) %>%
    group_by(.data$Region, .data$Data1, .data$Year)%>%
    summarise(Value=sum(.data$Value), .groups="drop")%>%
    group_by(.data$Region, .data$Year)%>%
    mutate(Total = sum(.data$Value),
           share = .data$Value/.data$Total)
  # ---------------------------------------------------------------------------
  # Calculate shares from additional region-specific data
  # - Plastics Europe data (2006-2020)
  # - China data (1978-2021, fill gaps by linear interpolation)
  # - US data (1960-2018, fill gaps by linear interpolation)
  # - calculate shares
  # ---------------------------------------------------------------------------
  eu <- readSource("PlasticsEurope", subtype="PlasticEoL_EU", convert=FALSE) %>% as.data.frame()
  cn_raw <- readSource("China_PlasticEoL", convert=FALSE)
  cn <- time_interpolate(cn_raw, interpolated_year = 1978:2021, integrate_interpolated_years=TRUE) %>% as.data.frame() %>%
    mutate(Data1 = case_when(.data$Data1=="Untreatment"~"Uncollected", TRUE ~ .data$Data1))
  us_raw <- readSource("US_EPA", convert=FALSE)
  us <- time_interpolate(us_raw, interpolated_year = 1960:2018, integrate_interpolated_years=TRUE) %>% as.data.frame()
  combined <- rbind(eu, cn, us) %>% select(-"Cell") %>%
    mutate(Year = as.integer(as.character(.data$Year)))%>%
    group_by(.data$Region, .data$Year)%>%
    mutate(Total = sum(.data$Value),
           share = .data$Value/.data$Total)
  # ---------------------------------------------------------------------------
  # replace OECD Eol shares where other data is available
  # recalculate Recycled, Incinerated and Landfilled shares as shares of total collected
  # calculate Collected share as 1-Uncollected
  # ---------------------------------------------------------------------------
  full_data <- left_join(oecd, combined, by=c("Region","Year","Data1")) %>%
    mutate(share = if_else(!is.na(.data$share.y), .data$share.y, .data$share.x))%>%
    group_by(.data$Region, .data$Year)%>%
    mutate(Total_collected = sum(.data$share[.data$Data1 != "Uncollected"]),
           share_new = case_when(.data$Data1 != "Uncollected" ~ .data$share/.data$Total_collected, TRUE~(1-.data$share)),
           Data1 = case_when(.data$Data1 == "Uncollected"~"Collected", TRUE~.data$Data1))%>%
    select("Region","Year","Data1","share_new")
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and apply regional-to-country mapping.
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  x <- as.magpie(full_data, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  # ---------------------------------------------------------------------------
  # Select data based on subtype
  # ---------------------------------------------------------------------------
  x <- switch(
    subtype,
    "Recycled" = mselect(x, Data1="Recycled"),
    "Landfilled" = mselect(x, Data1="Landfilled"),
    "Incinerated" = mselect(x, Data1="Incinerated"),
    "Collected" = mselect(x, Data1="Collected"),
    "All" = x,
    stop("Unsupported subtype: ", subtype)
  )
  getNames(x) <- NULL
  # ---------------------------------------------------------------------------
  # Prepare weight object
  #    - Use equal weights (1) for all country-fate combinations.
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  return(list(
    x           = x,
    weight      = weight,
    unit        = "ratio",
    description = "End-of-life fate ratios of plastic disaggregated to country level from OECD Plastics Outlook;
    EUR, USA and CHA are replaced by region-specific data from Plastics Europa, EPA and Chinese reports",
    note        = "dimensions: (Historic Time,Region,value)"
  ))
}


