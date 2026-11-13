#' Read Textile Exchange synthetic fibre data into a magpie object
#'
#' This function reads Textile Exchange (Materials Market Report 2025) data on
#' synthetic fibre production and its estimated regional distribution based on
#' Industrievereinigung Chemiefaser e.V. regional production volumes from an Excel file,
#' based on a specified subtype, and returns a magpie object.
#'
#' @param subtype Character string specifying the dataset:
#'        - "timeseries_by_type": global synthetic fibre production in Mt by fibre
#'          type and year (fibre, year, production_Mt).
#'        - "region_share": chemical fibre production shares by region and year,
#'          2007-2022, over the split WEU (Western Europe excl. DE), DEU, USA, JPN,
#'          PAK, IND, KOR, TWN, CHN and Other (region, year, share).
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
#' @importFrom magclass as.magpie
#'
readTextileExchange <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map subtype to Excel sheet and cell range
  params <- switch(subtype,
    "timeseries_by_type" = list(sheet = "timeseries_by_type", range = "A1:D59"),
    "region_share"       = list(sheet = "region_shares_pct",  range = "A3:K19"),
    stop("Invalid subtype: ", subtype)
  )

  raw_df <- readxl::read_excel(
    path  = "TextileExchange.xlsx",
    sheet = params$sheet,
    range = params$range
  )

  # ---------------------------------------------------------------------------
  # Convert to magpie object (global for the time series, regional for shares)
  magpie_data <- switch(subtype,
    "timeseries_by_type" = {
      raw_df$region <- "GLO"
      as.magpie(raw_df[, c("region", "year", "fibre", "incl_PlasticsEurope", "production_Mt")],
                spatial = "region", temporal = "year", data = "production_Mt")
    },
    "region_share" = {
      # wide (year x region) -> long (region, year, share)
      long_df <- tidyr::pivot_longer(raw_df, cols = -"Year", names_to = "region", values_to = "share")
      names(long_df)[names(long_df) == "Year"] <- "year"
      as.magpie(long_df[, c("region", "year", "share")],
                spatial = "region", temporal = "year", data = "share")
    },
    stop("Unsupported subtype: ", subtype)
  )

  return(magpie_data)
}
