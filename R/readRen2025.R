#' Read Ren et al. (2025) China plastic stocks and flows
#'
#' @description
#' Read the dynamic material-flow database for China (1978-2022) from Ren et al.
#' (2025, \doi{10.1038/s41597-025-06363-0}).
#' The \code{LongData} sheet holds one row per flow with the dimensions
#' \code{Stage} (M/P/U/W), \code{Process}, \code{Plastic} (20 polymer types),
#' \code{Product}, \code{Sector} and \code{Disposal}; a \code{/} marks a
#' dimension that is inactive for a given flow. The data is read into a magpie
#' object on the single country' \code{CHN}. Values are in \eqn{10^4} t.
#'
#' @return magpie object with the raw Ren et al. (2025) China plastic flows, with
#'   name dimension \code{stage.process.polymer.product.sector.disposal}.
#' @author Leonie Schweiger
#' @seealso [readSource()], [convertRen2025()]
#' @examples
#' \dontrun{
#' a <- readSource("Ren2025", convert = FALSE)
#' }
#' @importFrom magclass as.magpie getSets<-
readRen2025 <- function() {
  # ---------------------------------------------------------------------------
  # Read the raw long-format flow table
  # ---------------------------------------------------------------------------
  df <- readxl::read_excel("baseline_long_data.xlsx", sheet = "LongData")

  # ---------------------------------------------------------------------------
  # Build a magpie object on the single country CHN. The six category columns
  # become the name subdimensions; Value is the data column.
  # ---------------------------------------------------------------------------
  df <- data.frame(
    region   = "CHN",
    year     = df$Year,
    stage    = df$Stage,
    process  = df$Process,
    polymer  = df$Plastic,
    product  = df$Product,
    sector   = df$Sector,
    disposal = df$Disposal,
    value    = df$Value
  )

  x <- as.magpie(df, spatial = "region", temporal = "year", datacol = "value")

  return(x)
}
