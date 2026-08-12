#' Calculate synthetic rubber production by country
#'
#' Combines the IRSG regional synthetic rubber production (disaggregated to
#' country level via chemical energy consumption) with the global production
#' totals for the more recent years. For years in which only a global figure is
#' available, country production is obtained by multiplying the global total by
#' each country's production share from the closest year for which country data
#' exist.
#'
#' @return A list in \code{\link[madrat]{calcOutput}} format with synthetic
#'   rubber production by country and year.
#' @author Leonie Schweiger
#' @seealso [readIRSG()]
#' @examples
#' \dontrun{
#' a <- calcOutput("PlSyntheticRubber")
#' }
#' @importFrom magclass dimSums getYears setYears mbind
calcPlSyntheticRubber <- function() {
  # country-level production for the years with a regional split (2018-2020)
  countryProd <- readSource("IRSG", subtype = "regional", convert = TRUE)
  # global totals for the years with only a global figure (2021-2024)
  globalTotal <- readSource("IRSG", subtype = "global_total", convert = FALSE)
  getNames(globalTotal) <- NULL

  regionalYears <- getYears(countryProd, as.integer = TRUE)
  globalYears <- getYears(globalTotal, as.integer = TRUE)
  onlyGlobalYears <- setdiff(globalYears, regionalYears)

  # for each global-only year, apply the country shares of the closest year with data
  shares <- countryProd / dimSums(countryProd, dim = 1)
  target_years <- union(regionalYears, globalYears)
  shares <- toolInterpolate(shares, years = target_years, extrapolate = TRUE)
  globalProd <- globalTotal[, onlyGlobalYears, ] * shares[, onlyGlobalYears, ]

  # 2018-2020 taken directly, 2021-2024 derived from global totals * closest-year shares
  x <- mbind(countryProd, globalProd)

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt",
    description = paste(
      "Synthetic rubber production by country and year from IRSG. Regional",
      "production (2018-2020) is disaggregated to countries by chemical energy",
      "consumption; years with only a global total (2021-2024) are split using",
      "each country's production share from the closest year with country data."
    ),
    note = "dimensions: (Time,Region,value)",
    min = 0
  ))
}
