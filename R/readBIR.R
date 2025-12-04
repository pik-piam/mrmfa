#' Read BIR (Bureau of International Recycling) data
#'
#' @description
#' Reads BIR data on steel scrap shares and consumption.
#'
#' @author Merlin Jo Hosak
#' @param subtype Type of data to read. Options: "scrapShare", "scrapConsumption"
#'
readBIR <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "scrapShare" = function() {
      path <- file.path(".", "v1.0", "BIR_ScrapShareProduction.xlsx")
      df <- readxl::read_excel(path, sheet = "Data")
      x <- as.magpie(df, spatial = "Scrap share in production")
      getNames(x) <- NULL
      return(x)
    },
    "scrapConsumption" = function() {
      path <- file.path(".", "v1.0", "BIR_ScrapConsumption_Ammended.xlsx")
      df <- readxl::read_excel(path, sheet = "Data", skip = 1)
      x <- as.magpie(df, spatial = "region")
      getNames(x) <- NULL
      x <- x * 1e6 # convert from Mt to t
      return(x)
    }
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
