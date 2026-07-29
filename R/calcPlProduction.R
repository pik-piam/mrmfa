#' Calculate combined plastics, fibre and rubber production by country
#'
#' Merges the three country-level production series of the plastics subsystem
#' into a single magpie object with a \code{type} dimension
#' (\code{Fibre}, \code{Rubber}, \code{Plastics}) and backcasts each series to
#' 1950. Synthetic fibre comes from Textile Exchange
#' (\code{\link{calcPlSyntheticFibre}}), synthetic rubber from IRSG
#' (\code{\link{calcPlSyntheticRubber}}) and conventional plastics from Plastics
#' Europe (\code{\link{calcPlPlasticsEurope}}).
#'
#' Each source only covers recent years, so every series is backcast
#' independently along the total plastics trend: first with the OECD regional
#' plastic use (1990-2019, \code{readSource("OECD_Plastic",
#' "Use_1990-2019_region")}) and then with the Geyer et al. 2017 global
#' production (1950-2015, \code{readSource("Geyer", "Prod_1950-2015")}). Since
#' the fibre series only starts in 2020 - one year after the OECD reference ends
#' - its 2019 value is assumed equal to 2020 so that it overlaps the OECD
#' reference and can be backcast.
#'
#' @return A list in \code{\link[madrat]{calcOutput}} format with plastics,
#'   fibre and rubber production by country, type and year (1950-2024).
#' @author Leonie Schweiger
#' @seealso [calcPlSyntheticFibre()], [calcPlSyntheticRubber()],
#'   [calcPlPlasticsEurope()], [toolBackcastByReference()]
#' @examples
#' \dontrun{
#' a <- calcOutput("PlProduction")
#' }
#' @importFrom magclass dimSums getYears setYears getNames getNames<- getItems
#' @importFrom magclass getSets<- collapseDim mbind new.magpie magpiesort
#' @export
calcPlProduction <- function() {
  # ---------------------------------------------------------------------------
  # Read the three country-level production series
  # ---------------------------------------------------------------------------
  fibre    <- calcOutput("PlSyntheticFibre",  aggregate = FALSE)
  rubber   <- calcOutput("PlSyntheticRubber", aggregate = FALSE)
  plastics <- calcOutput("PlPlasticsEurope",  aggregate = FALSE)

  # ---------------------------------------------------------------------------
  # Build the two backcasting references (total plastics trend)
  # ---------------------------------------------------------------------------
  oecdTotal <- readSource("OECD_Plastic", subtype = "Use_1990-2019_region")
  # clean set names
  getSets(oecdTotal, fulldim = FALSE) <- c("region", "year", "data")
  geyer <- readSource("Geyer", subtype = "Prod_1950-2015", convert = FALSE)

  # ---------------------------------------------------------------------------
  # Backcast each source independently: OECD (-> 1990) then Geyer (-> 1950) and
  # trim to the last year covered by all types (sources may end in different years)
  # ---------------------------------------------------------------------------
  lastYear <- min(max(getYears(fibre)), max(getYears(rubber)), max(getYears(plastics)))
  backcast <- function(x) {
    x <- toolBackcastByReference(x, oecdTotal)
    x <- toolBackcastByReference(x, geyer)
    magpiesort(x)
    x <- x[, getYears(x) < lastYear, ]
  }
  fibre    <- backcast(fibre)
  rubber   <- backcast(rubber)
  plastics <- backcast(plastics)

  # ---------------------------------------------------------------------------
  # subtract fibres included in PlasticsEurope from PlasticsEurope,
  # then merge into the type dimension.
  # ---------------------------------------------------------------------------
  plastics <- plastics - collapseDim(fibre[, , "TRUE"])
  fibre <- dimSums(fibre, dim = 3)
  prep <- function(x, typeName) {
    x <- collapseDim(x)
    getNames(x) <- typeName
    x
  }
  fibre    <- prep(fibre, "Fibre")
  rubber   <- prep(rubber, "Rubber")
  plastics <- prep(plastics, "Plastics")
  x <- mbind(fibre, rubber, plastics)
  names(dimnames(x))[3] <- "type"

  # convert Mt -> t
  x <- x * 1e6

  return(list(
    x = x,
    weight = NULL,
    unit = "t",
    description = paste(
      "Plastics (Plastics Europe), synthetic fibre (Textile Exchange) and",
      "synthetic rubber (IRSG) production by country and type, 1950-2024. Each",
      "source is backcast independently along the total plastics trend, first",
      "with OECD regional plastic use (1990-2019) and then with Geyer et al.",
      "2017 global production (1950-2015). Fibre 2019 is assumed equal to 2020",
      "so it overlaps the OECD reference."
    ),
    note = "dimensions: (Time,Region,Type,value)"
  ))
}
