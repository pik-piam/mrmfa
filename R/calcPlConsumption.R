#' Calculate Country-Level Total Plastic Use
#'
#' Load OECD regional plastic use data and trade/production inputs,
#' adjust for domestic demand, normalize baselines, and aggregate to country level.
#'
#' @author Qianzhi Zhang
#' @importFrom dplyr if_else
calcPlConsumption <- function() {
  # ---------------------------------------------------------------------------
  # Load & clean OECD regional use data (1990–2019)
  # ---------------------------------------------------------------------------
  use_region <- calcOutput(
    "PlOECD", subtype = "Use_1990-2019_region", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::select(-"Cell", -"Data1", -"Data2") %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year)))

  # ---------------------------------------------------------------------------
  # Replace modelled OECD data with actual data from PlasticsEurope where available
  # - regions to be replaced are CHA, EUR and USA
  # - actual data is production and not consumption, therefore it has to be corrected by net imports
  # - as PlasticsEurope lists data for "North America", the CAN demand has to be subtracted to obtain USA demand
  # - actual data is only after 2005, so data before 2005 is calculated from OECD data scaled by 2005 data
  # ---------------------------------------------------------------------------
  # filter OECD data that will be replaced and get 2005 value for scaling
  target_regions <- c("CHA", "EUR", "USA", "CAN")
  use_target <- use_region %>%
    dplyr::filter(.data$Region %in% target_regions) %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::mutate(
      baseline2005 = .data$Value[.data$Year == 2005],
      ratio        = .data$Value / .data$baseline2005
    ) %>%
    dplyr::ungroup()
  use_other <- use_region %>% dplyr::filter(!.data$Region %in% target_regions)

  prod_region_map <- c("China" = "CHA", "EU27+3" = "EUR", "North America" = "USA")
  prod_data <- readSource("PlasticsEurope", subtype="PlasticProduction_region", convert=FALSE) %>%
    as.data.frame() %>%
    dplyr::mutate(
      Year = as.integer(as.character(.data$Year)),
      Region = dplyr::recode(.data$Region, !!!prod_region_map)
    ) %>%
    dplyr::filter(.data$Region %in% target_regions)

  # Calculate UNCTAD net imports for target regions
  trade_data_region <- calcOutput("PlUNCTAD", subtype="Final_Region", aggregate=TRUE)%>%
    as.data.frame()%>%
    dplyr::filter(.data$Region %in% target_regions, .data$Region !="USA") # USA is included in trade_data_country
  trade_data_country <- calcOutput("PlUNCTAD", subtype="Final_Country", aggregate=FALSE)%>%
    as.data.frame()%>%
    dplyr::filter(.data$Region %in% target_regions)
  trade_data <- rbind(trade_data_region, trade_data_country) %>% pivot_wider(names_from="Data1", values_from="Value") %>%
    dplyr::mutate(net_import = .data$Imports - .data$Exports,
                  Year = as.integer(as.character(.data$Year))) %>%
    dplyr::select("Region","Year","net_import")

  # Compute regional total use = production + net imports
  use_calc <- prod_data %>%
    dplyr::left_join(trade_data, by = c("Region", "Year")) %>%
    tidyr::replace_na(list(net_import = 0)) %>%
    dplyr::mutate(use = .data$Value + .data$net_import)

  # Calculate Canada's production by subtracting net imports from consumption (OECD data)
  can_data <- calcOutput(
    "PlOECD", subtype = "Use_1990-2019_region", aggregate = FALSE
  ) %>%
    as.data.frame() %>%
    dplyr::select(-"Cell", -"Data1", -"Data2") %>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year))) %>%
    dplyr::filter(.data$Region == "CAN") %>%
    dplyr::left_join(trade_data, by = c("Region", "Year")) %>%
    tidyr::replace_na(list(net_import = 0)) %>%
    dplyr::transmute(.data$Year, can_prod = .data$Value - .data$net_import)

  # Merge & update target region values
  updated_target <- use_target %>%
    dplyr::left_join(use_calc, by = c("Region", "Year")) %>%
    dplyr::left_join(can_data, by = "Year") %>%
    dplyr::mutate(
  # subtract Canada's production from North America production (net-import corrected for the USA) to get USA demand
      use_adj = if_else(.data$Region == "USA", .data$use - .data$can_prod, .data$use)
    ) %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::mutate(
  # data before 2005 is calculated from OECD data scaled by 2005 data
      use_adj_2005 = .data$use_adj[.data$Year == 2005],
      Value        = if_else(.data$Year >= 2005, .data$use_adj, .data$use_adj_2005 * .data$ratio)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(names(use_region)))
  final_region <- dplyr::bind_rows(updated_target, use_other)

  # ---------------------------------------------------------------------------
  # for the EU, actual plastics consumption data is available:
  # 55.4 Mt in 2018 according to Plastics Europe 2024 circular economy report
  # -> use this value instead of the data calculated from production and net imports
  #    and scale values from other years accordingly
  # ---------------------------------------------------------------------------
  eur_2018_value <- final_region %>%
    dplyr::filter(.data$Region == "EUR", .data$Year == 2018) %>%
    dplyr::pull(.data$Value)
  final_region <- final_region %>%
    dplyr::mutate(
      Value = if_else(
        .data$Region == "EUR",
        .data$Value * 55.4 / eur_2018_value,
        .data$Value
      )
    )

  # ---------------------------------------------------------------------------
  # Aggregate to country level by GDP weights
  # ---------------------------------------------------------------------------
  map_df <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mappingfolder"
  )
  magpie_x <- as.magpie(final_region, spatial = 1, temporal = 2)
  gdp_ssp2 <- calcOutput(
    "GDP", scenario="SSP2", average2020 = FALSE, aggregate = FALSE
  )[, paste0("y", 1990:2019), "SSP2"]
  x_final <- toolAggregate(
    magpie_x, rel = map_df, dim = 1,
    from = "RegionCode", to = "CountryCode",
    weight = gdp_ssp2[unique(map_df$CountryCode), , ]
  )

  # ---------------------------------------------------------------------------
  # Return final output
  # ---------------------------------------------------------------------------
  return(list(
    x           = x_final,
    weight      = NULL,
    unit        = "Mt Plastic",
    description = "Country-level plastic use aggregated from OECD and trade sources.",
    note        = "dimensions: (Historic Time,Region,value)"
  ))
}


