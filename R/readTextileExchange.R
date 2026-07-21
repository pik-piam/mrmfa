#' Read Textile Exchange synthetic fibre data into a magpie object
#'
#' This function reads Textile Exchange (Materials Market Report 2025) data on
#' synthetic fibre production and its estimated regional distribution based on
#' Credence Research market data on monoethylene glycol from an Excel file,
#' based on a specified subtype, and returns a magpie object.
#'
#' @param subtype Character string specifying the dataset:
#'        - "timeseries_by_type": global synthetic fibre production in Mt by fibre
#'          type and year (fibre, year, production_Mt).
#'        - "region_share": regional production shares (\%) by fibre and year
#'          (region, fibre, share_pct, year).
#'
#' @return magpie object of the Textile Exchange data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "TextileExchange", subtype = "timeseries_by_type", convert = FALSE)
#' }
#' @importFrom readxl read_excel
#' @importFrom magclass as.magpie getComment<-
#'
readTextileExchange <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map subtype to Excel sheet and cell range
  params <- switch(subtype,
    "timeseries_by_type" = list(sheet = "timeseries_by_type", range = "A1:C16"),
    "region_share"       = list(sheet = "region_share",       range = "A1:D6"),
    stop("Invalid subtype: ", subtype)
  )

  raw_df <- read_excel(
    path  = "TextileExchange.xlsx",
    sheet = params$sheet,
    range = params$range
  )

  # ---------------------------------------------------------------------------
  # Convert to magpie object (global for the time series, regional for shares)
  magpie_data <- switch(subtype,
    "timeseries_by_type" = {
      raw_df$region <- "GLO"
      as.magpie(raw_df[, c("region", "year", "fibre", "production_Mt")],
                spatial = 1, temporal = 2)
    },
    "region_share" = as.magpie(raw_df[, c("region", "year", "fibre", "share_pct")],
                               spatial = 1, temporal = 2),
    stop("Unsupported subtype: ", subtype)
  )

  magpie_data[is.na(magpie_data)] <- 0
  getComment(magpie_data) <- subtype

  return(magpie_data)
}
