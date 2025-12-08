#' Calculate UNCTAD Plastic Trade Flows
#'
#' Read and aggregate UNCTAD plastic trade data (imports/exports) for multiple plastic categories
#' at regional or country level, with overrides for key economies.
#'
#' @param subtype Character; scenario to read. Options:
#'   \itemize{
#'     \item Final_Region         - Imports/exports of final plastics by region
#'     \item Final_Country        - Imports/exports of final plastics by country
#'     \item Waste_Region         - Plastic waste flows by region
#'     \item Waste_Country        - Plastic waste flows by country
#'     \item Primary_Region       - Imports/exports of primary plastics by region
#'     \item Primary_Country      - Imports/exports of primary plastics by country
#'     \item Intermediate_Region  - Imports/exports of intermediate forms of plastic by region
#'     \item Intermediate_Country - Imports/exports of intermediate forms of plastic by country
#'     \item Manufactured_Region  - Imports/exports of intermediate manufactured plastic goods by region
#'     \item Manufactured_Country - Imports/exports of intermediate manufactured plastic goods by country
#'   }
#'
#' @author Qianzhi Zhang
calcPlUNCTAD <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Setup: files, mappings, and weights
  # ---------------------------------------------------------------------------
  data <- readSource("UNCTAD", convert = TRUE)
  data_regional <- readSource("UNCTAD", convert = FALSE)
  recode_regions <- c(
    "European Union (2020 \u2026)" = "EUR",
    "China" = "CHA",
    "United States of America" = "USA"
  )
  map_df <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  gdp_ssp2 <- calcOutput("CoGDP1900To2150", scenario="SSP2", perCapita=FALSE, aggregate=FALSE)[, paste0("y", 2005:2023),]

  # ---------------------------------------------------------------------------
  # Helper: build region-level flows for given product and tag
  # ---------------------------------------------------------------------------
  build_region_flow <- function(prod_label, data2_tag) {
    # get data on country level and aggregate to region
    m_r <- toolAggregate(data,
      rel = map_df, dim = 1,
      from = "CountryCode", to = "RegionCode"
    )
    df_r <- as.data.frame(m_r) %>%
      dplyr::filter(.data$Data2 == prod_label) %>%
      dplyr::mutate(Year = as.integer(as.character(.data$Year))) %>%
      dplyr::select("Region", "Year", "Data1", "Value")
    # get data directly on regional level and use this instead of aggregated data if available
    df_ov <- data_regional %>%
      as.data.frame() %>%
      dplyr::filter(
        .data$Region %in% names(recode_regions),
        .data$Data2 == prod_label
      ) %>%
      dplyr::mutate(
        Region = dplyr::recode(.data$Region, !!!recode_regions),
        Year = as.integer(as.character(.data$Year))
      ) %>%
      dplyr::select("Region", "Year", "Data1", "Value")
    df_f <- df_r %>%
      dplyr::left_join(df_ov, by = c("Region", "Year", "Data1"), suffix = c("", ".new")) %>%
      dplyr::mutate(
        Value = dplyr::if_else(!is.na(.data$Value.new), .data$Value.new, .data$Value),
        Data2 = data2_tag
      ) %>%
      dplyr::select("Region", "Year", "Data1", "Data2", "Value")
    m_f <- as.magpie(df_f, spatial = 1, temporal = 2)
    m_f[is.na(m_f)] <- 0
    x <- toolAggregate(m_f,
      rel = map_df, dim = 1,
      from = "RegionCode", to = "CountryCode",
      weight = gdp_ssp2[unique(map_df$CountryCode), , ]
    )
    return(x / 1000) # thousand tons to Mt
  }

  # ---------------------------------------------------------------------------
  # Dispatch region-level subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Final_Region") {
    return(list(
      x           = build_region_flow("Final manufactured plastics goods", "Final Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of final plastics"
    ))
  }
  if (subtype == "Primary_Region") {
    return(list(
      x           = build_region_flow("Plastics in primary forms", "Primary Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of primary plastics"
    ))
  }
  if (subtype == "Intermediate_Region") {
    return(list(
      x           = build_region_flow("Intermediate forms of plastic", "Intermediate Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of intermediate plastic forms"
    ))
  }
  if (subtype == "Manufactured_Region") {
    return(list(
      x           = build_region_flow("Intermediate manufactured plastic goods", "Manufactured Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of manufactured plastic goods"
    ))
  }

  # ---------------------------------------------------------------------------
  # Helper: build country-level flows for given product
  # ---------------------------------------------------------------------------
  build_country_flow <- function(prod_label, data2_tag) {
    x <- data[, , grepl(prod_label, getItems(data, dim = 3))]
    getItems(x, dim = 3.2) <- data2_tag
    return(x / 1000) # thousand tons to Mt
  }

  # ---------------------------------------------------------------------------
  # Dispatch country-level subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Final_Country") {
    return(list(
      x           = build_country_flow("Final manufactured plastics goods", "Final Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of final plastics"
    ))
  }
  if (subtype == "Primary_Country") {
    return(list(
      x           = build_country_flow("Plastics in primary forms", "Primary Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of primary plastics"
    ))
  }
  if (subtype == "Intermediate_Country") {
    return(list(
      x           = build_country_flow("Intermediate forms of plastic", "Intermediate Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of intermediate plastic forms"
    ))
  }
  if (subtype == "Manufactured_Country") {
    return(list(
      x           = build_country_flow("Intermediate manufactured plastic goods", "Manufactured Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of manufactured plastic goods"
    ))
  }

  # ---------------------------------------------------------------------------
  # Dispatch waste subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Waste_Region") {
    x <- build_region_flow("Plastic waste", "Plastic Waste")
    return(list(
      x           = x,
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level plastic waste flows"
    ))
  }
  if (subtype == "Waste_Country") {
    x <- build_country_flow("Plastic waste")
    return(list(
      x           = x,
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level plastic waste flows"
    ))
  }

  # ---------------------------------------------------------------------------
  # Error handling for unknown subtype
  # ---------------------------------------------------------------------------
  stop("Unknown subtype: ", subtype)
}
