#' Read Wuppertal Institute GreenFeed EoL parameters into a magpie object
#'
#' @description
#' Read the Wuppertal Institute "GreenFeed" (GF) baseline dataset on plastics
#' end-of-life (EoL) treatment for the EU. The data is stored in ODYM-RECC
#' parameter files; this reader uses the long-format \code{Values_Master} sheet
#' of each file. Three datasets are available via \code{subtype}:
#'
#' \describe{
#'   \item{\code{"CollectionRate"}}{Collection rate of EoL plastics (share of
#'     generated waste that is collected). Reported per region and sector; the
#'     \code{product} column carries all polymers but the value is identical
#'     across them (no polymer differentiation).}
#'   \item{\code{"SortingRate"}}{Share of collected waste sorted into each fate
#'     (\code{Mechanical recycling}, \code{Landfill}, \code{Incineration}), per
#'     region, sector and polymer.}
#'   \item{\code{"RecyclingYield"}}{Conversion rate of sorted mechanical-recycling
#'     waste into secondary raw material (Granulate), per region, sector and
#'     polymer.}
#' }
#'
#' The GF data resolves 5 EU sub-regions (\code{East}, \code{Germany},
#' \code{North}, \code{South}, \code{West}); only the 7 aggregate sectors are
#' kept (the detailed sub-sectors such as "PET beverage bottles" are dropped).
#' Read with \code{convert = FALSE} to obtain the raw GF resolution, or with the
#' default \code{convert = TRUE} to map to remind-mfa dimensions and ISO
#' countries (see \code{\link{convertWI_GreenFeed}}).
#'
#' @param subtype One of \code{"CollectionRate"}, \code{"SortingRate"},
#'   \code{"RecyclingYield"}.
#' @return magpie object of the requested GF EoL rate at GF sub-region
#'   resolution, name dimension \code{sector.polymer} (plus \code{waste} for
#'   \code{"SortingRate"}).
#' @author Leonie Schweiger
#' @seealso \code{\link{convertWI_GreenFeed}}, \code{\link{calcPlEoLGreenFeed}}
#' @examples
#' \dontrun{
#' a <- readSource("WI_GreenFeed", subtype = "SortingRate", convert = FALSE)
#' }
#' @importFrom magclass as.magpie getComment<-
readWI_GreenFeed <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map subtype to the source file
  # ---------------------------------------------------------------------------
  file <- switch(subtype,
    "CollectionRate" = "ODYM_GF_baseline_EoLCollectionRate.xlsx",
    "SortingRate"    = "ODYM_GF_baseline_SortingRate.xlsx",
    "RecyclingYield" = "ODYM_GF_baseline_RecyclingConversionRate.xlsx",
    stop("Invalid subtype: ", subtype)
  )

  # The 7 aggregate GF sectors (the detailed sub-sectors are dropped).
  aggSectors <- c(
    "Packaging", "Building and Construction", "Automotive",
    "Electrical and Electronics", "Household, Leisure, Sports",
    "Agriculture", "Others"
  )

  # ---------------------------------------------------------------------------
  # Read the long-format Values_Master sheet and reduce to the needed columns
  # ---------------------------------------------------------------------------
  raw <- readxl::read_excel(file, sheet = "Values_Master") %>% as.data.frame()
  raw <- raw[raw$sector %in% aggSectors, ]
  names(raw)[names(raw) == "product"] <- "polymer"

  # keep only the dimension columns and the value ("waste" only for SortingRate)
  cols <- c("region", "time", "sector", "polymer", intersect("waste", names(raw)), "value")
  df <- raw[, cols]

  # ---------------------------------------------------------------------------
  # Build the magpie object (region x time x sector.polymer[.waste])
  # ---------------------------------------------------------------------------
  x <- as.magpie(df, spatial = "region", temporal = "time", datacol = "value")

  return(x)
}
