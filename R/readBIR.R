#' Read BIR (Bureau of International Recycling) data
#' @description
#' Reads BIR data on steel scrap shares and consumption.
#'
#' @author Merlin Jo Hosak
#' @param subtype Type of data to read. Options: "scrapShare", "scrapConsumption", 
#' "scrapConsumptionEU", "scrapConsumptionWorld"
#' @export
readBIR <- function(subtype = "scrapShare") {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "scrapShare" = function() {
      path <- paste0("./v1.0/BIR_ScrapShareProduction.xlsx")
      df <- readxl::read_excel(path, sheet = "Data")
      scrapShares <- as.magpie(df, spatial = "Scrap share in production")

      return(scrapShares)
    },
    "scrapConsumption" = function() {
      scrapConsumption <- toolReadBIRscrapConsumption()
      countries <- getItems(scrapConsumption, dim = 1)
      getItems(scrapConsumption, dim = 1) <- toolCountry2isocode(
        countries,
        ignoreCountries = c("EU 28", "World")
      )

      # remove NA rows
      scrapConsumption <- scrapConsumption[!is.na(getItems(scrapConsumption, dim = 1)), , ]

      # convert unit
      scrapConsumption <- scrapConsumption * 1e6 # convert from Mt to t

      return(scrapConsumption)
    },
    "scrapConsumptionEU" = function() {
      scrapConsumption <- toolReadBIRscrapConsumption()
      scrapConsumptionEU <- scrapConsumption[getItems(scrapConsumption, dim = 1) == "EU 28", , ]
      scrapConsumptionEU <- scrapConsumptionEU * 1e6 # convert from Mt to t

      return(scrapConsumptionEU)
    },
    "scrapConsumptionWorld" = function() {
      scrapConsumption <- toolReadBIRscrapConsumption()
      scrapConsumptionWorld <- scrapConsumption[getItems(scrapConsumption, dim = 1) == "World", , ]
      scrapConsumptionWorld <- scrapConsumptionWorld * 1e6 # convert from Mt to t
      getItems(scrapConsumptionWorld, dim = 1) <- "GLO"

      return(scrapConsumptionWorld)
    },
    NULL
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

toolReadBIRscrapConsumption <- function() {
  path <- "./v1.0/BIR_ScrapConsumption_Ammended.xlsx"
  df <- readxl::read_excel(path, sheet = "Data", skip = 1)
  scrapConsumption <- as.magpie(df, spatial = "region")

  return(scrapConsumption)
}
