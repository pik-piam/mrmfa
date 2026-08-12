#' Read IRSG synthetic rubber data into a magpie object
#'
#' This function reads International Rubber Study Group (IRSG) data on synthetic
#' rubber (SR) production from an Excel file, based on a specified subtype, and
#' returns a magpie object.
#'
#' @param subtype Character string specifying the dataset:
#'        - "global_total": global synthetic rubber production in Mt by year
#'          (year, global_SR_production_Mt). Global data (region GLO).
#'        - "regional": regional synthetic rubber production in Mt by region and
#'          year (region, year, regional_SR_production_Mt).
#'          Regions are Asia, Europe, Americas and Africa (IRSG grouping).
#'
#' @return magpie object of the IRSG synthetic rubber data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "IRSG", subtype = "regional", convert = FALSE)
#' }
#' @importFrom readxl read_excel
#' @importFrom magclass as.magpie getComment<-
#'
readIRSG <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map subtype to Excel sheet and cell range (only the required columns)
  params <- switch(subtype,
    "global_total" = list(sheet = "global_total", range = "A1:B5"),
    "regional"     = list(sheet = "regional",     range = "A1:C13"),
    stop("Invalid subtype: ", subtype)
  )

  rawData <- read_excel(
    path  = "IRSG_rubber.xlsx",
    sheet = params$sheet,
    range = params$range
  )

  # ---------------------------------------------------------------------------
  # Convert to magpie object (global for the totals, regional for the split)
  magpieData <- switch(subtype,
    "global_total" = {
      rawData$region <- "GLO"
      as.magpie(rawData[, c("region", "year", "global_SR_production_Mt")],
                spatial = 1, temporal = 2)
    },
    "regional" = as.magpie(rawData[, c("region", "year", "regional_SR_production_Mt")],
                           spatial = 1, temporal = 2),
    stop("Unsupported subtype: ", subtype)
  )

  magpieData[is.na(magpieData)] <- 0
  getComment(magpieData) <- subtype

  return(magpieData)
}
