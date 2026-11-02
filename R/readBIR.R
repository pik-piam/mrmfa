#' Read BIR (Bureau of International Recycling) data
#'
#' @description
#' Reads BIR data on steel scrap consumption.
#'
#' @author Merlin Jo Hosak
#' @param subtype Type of data to read. Options: "scrapConsumption"
#'
readBIR <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "scrapConsumption" = function() {
      path <- file.path(".", "v1.1", "BIR_ScrapConsumption.xlsx")
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
