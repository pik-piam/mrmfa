#' Convert OECD Global Plastics Outlook Data to a MagPIE Object
#'
#' Convert OECD Global Plastics Outlook .xlsx data into a MagPIE object
#' aggregated to ISO country level
#'
#' @param x MagPIE object containing OECD_Plastic data at regional resolution.
#' @return MagPIE object of the OECD_Plastic data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertOECD_Plastic(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertOECD_Plastic <- function(x) {
  # ---------------------------------------------------------------------------
  # Extract subtype information and validate
  #    - Retrieve comment string and split to derive subtype components.
  # ---------------------------------------------------------------------------
  comment_str <- getComment(x)
  parts <- strsplit(strsplit(comment_str, " ")[[1]][3], "_")[[1]]
  if (length(parts) < 3) {
    stop("Subtype in comment must have three parts, e.g. 'Use_1990-2019_region'.")
  }
  subtype <- paste(parts[1], parts[2], parts[3], sep = "_")  # e.g. "Use_1990-2019_region"
  
  # ---------------------------------------------------------------------------
  # Disaggregate regional data to country level
  #    - Use different mappings and GDP weighting per subtype case.
  # ---------------------------------------------------------------------------
  if (subtype == "Use_1990-2019_region") {
    # GDP weights for 1990-2019
    GDP <- calcOutput("GDP", scenario="SSP2", average2020 = FALSE,
                      naming = "scenario", aggregate = FALSE)[,
                                                              paste0("y", 1990:2019), "SSP2"]
    # Regional-to-country mapping
    map <- toolGetMapping("regionmappingOECDPlastic.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$OECDPlasticReg != "rest")
    # Aggregate to ISO country code
    x <- toolAggregate(x, rel = map, dim = 1,
                       from = "OECDPlasticReg", to = "CountryCode",
                       weight = GDP[unique(map$CountryCode), , ])
    
  } else if (subtype %in% c("Use_2019_region", "WasteType_2019_region")) {
    # Sectoral mappings for plastics use and manufacturing
    use_map <- toolGetMapping("structuremappingPlasticUse.csv", type = "sectoral", where = "mrindustry")
    manu_map <- toolGetMapping("structuremappingPlasticManu.csv", type = "sectoral", where = "mrindustry")
    x <- toolAggregate(x, rel = use_map, dim = 3.2, from = "Source", to = "Target")
    x <- toolAggregate(x, rel = manu_map, dim = 3.1, from = "Source", to = "Target")
    # GDP weights for 2019
    GDP2019 <- calcOutput("GDP", scenario="SSP2", average2020 = FALSE,
                          naming = "scenario", aggregate = FALSE)[, 2019, "SSP2"]
    region_map <- toolGetMapping("regionmappingOECDPlastic.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$OECDPlasticReg != "rest")
    x <- toolAggregate(x, rel = region_map, dim = 1,
                       from = "OECDPlasticReg", to = "CountryCode",
                       weight = GDP2019[unique(region_map$CountryCode), ])
    
  } else if (subtype == "Use_1990-2019_world") {
    # Sectoral mappings for global trends
    use_map16 <- toolGetMapping("structuremappingPlasticUse_16.csv", type = "sectoral", where = "mrindustry")
    manu_map16 <- toolGetMapping("structuremappingPlasticManu_16.csv", type = "sectoral", where = "mrindustry")
    x <- toolAggregate(x, rel = use_map16, dim = 3.2, from = "Source", to = "Target")
    x <- toolAggregate(x, rel = manu_map16, dim = 3.1, from = "Source", to = "Target")
    # GDP weights for 1990-2019
    GDP <- calcOutput("GDP", scenario="SSP2", average2020 = FALSE,
                      naming = "scenario", aggregate = FALSE)[,
                                                              paste0("y", 1990:2019), "SSP2"]
    region_map <- toolGetMapping("regionmappingOECDPlastic.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$OECDPlasticReg != "rest")
    x <- toolAggregate(x, rel = region_map, dim = 1,
                       from = "OECDPlasticReg", to = "CountryCode",
                       weight = GDP[unique(region_map$CountryCode), , ])
    
  } else if (subtype == "WasteEOL_1990-2019_region") {
    # GDP weights for 1990-2019
    GDP <- calcOutput("GDP", scenario="SSP2", average2020 = FALSE,
                      naming = "scenario", aggregate = FALSE)[,
                                                              paste0("y", 1990:2019), "SSP2"]
    region_map <- toolGetMapping("regionmappingOECDPlastic.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$OECDPlasticReg != "rest")
    x <- toolAggregate(x, rel = region_map, dim = 1,
                       from = "OECDPlasticReg", to = "CountryCode",
                       weight = GDP[unique(region_map$CountryCode), , ])
    
  } else {
    # -------------------------------------------------------------------------
    # Error handling for unsupported subtypes
    # -------------------------------------------------------------------------
    stop("Invalid subtype combination: ", subtype)
  }
  
  # ---------------------------------------------------------------------------
  # Finalize data
  #    - Fill missing country entries with zeros for completeness.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  return(x)
}
