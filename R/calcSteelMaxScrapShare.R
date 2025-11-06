#' Calc max steel scrap share in production
#' @description
#' Estimate the maximum share steel scrap can have in steel production
#' based on data by the Bureau of International Recycling (BIR),
#' see \link[readBIR]{readBIR} and the folder 'BIR' in sources.
#' @param subtype Data source used, current default is BIR (Bureau of
#' International Recycling).
#' @author Merlin Jo Hosak
#' @export
calcSteelMaxScrapShare <- function(subtype = "BIR") {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "BIR" = function() {
      scrapShares <- readSource("BIR", subtype = "scrapShare", convert = FALSE)

      # Remove turkey as it is an outlier
      scrapShares <- scrapShares[!getRegions(scrapShares) == "Turkey", , ]

      # Assume the max to be the 95th percentile of remaining data.
      maxScrapShare <- magpply(X = scrapShares, MARGIN = 3, FUN = function(x) quantile(x, 0.95, na.rm = TRUE))

      # Finalize

      getSets(maxScrapShare) <- c("Region", "Year", "Parameter")
      getItems(maxScrapShare, dim = 1) <- "GLO"
      getNames(maxScrapShare) <- "Max steel scrap share"

      final <- list(
        x = maxScrapShare,
        weight = NULL,
        unit = 1,
        description = "Maximum scrap share in production"
      )

      return(final)
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
