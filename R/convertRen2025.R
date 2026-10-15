#' Convert Ren et al. (2025) China plastic flows to remind-mfa categories
#'
#' @description
#' Map the raw Ren et al. (2025) flows onto the remind-mfa categories.
#' The 20 source polymers are mapped to the base polymer set (\code{polymermappingRen2025.csv}),
#' keeping \code{PA} (polyamide) as its own category so that \code{\link{calcPlRen2025}}
#' can later split off the polyamide fibre share. The nine end-use sectors are mapped to the eight
#' remind-mfa end uses (\code{sectormappingRen2025.csv}) and the disposal categories
#' are renamed to the mrmfa vocabulary (\code{Recycled}, \code{Incinerated},
#' \code{Landfilled}, \code{Untreated}). Countries other than China are filled with 0.
#'
#' The sector mapping is resolved per (sector, product, polymer), most specific
#' match first, so that Ren's \code{Agriculture} and \code{Other sectors} -
#' which have no Gao & Cabrera-Serrenho (2025) counterpart - follow Gao's
#' categories where the product is identifiable:
#' \itemize{
#'   \item Agriculture pipes, plates and profiles -> Buildings & Construction
#'     (irrigation/greenhouse infrastructure; Gao puts 70\% of PVC there).
#'   \item Agriculture woven goods -> Packaging (sacks; Ren puts most woven goods there).
#'   \item Other agriculture (mostly film) -> Others (Gao has no agriculture sector).
#'   \item Other-sector containers, household plastics, synthetic leather and
#'     EPS foam -> Consumer & Institutional (storage boxes, shoes, bags, upholstery).
#'   \item Remaining other-sector products, incl. PU foam -> Others (unspecific;
#'     Gao puts 23\% of PUR, i.e. furniture/bedding foam, in Others).
#' }
#' Product-resolved flows exist only for some stages (e.g. use-stage
#' \code{domestic}); flows without a product use the sector-level default.
#'
#' @param x magpie object with the raw Ren et al. (2025) China plastic flows.
#' @inherit readRen2025 return
#' @author Leonie Schweiger
#' @seealso \code{\link{readRen2025}}
#' @importFrom magclass getItems getItems<- as.magpie
#' @importFrom dplyr left_join filter select rename mutate coalesce group_by across all_of summarise
convertRen2025 <- function(x) {
  # ---------------------------------------------------------------------------
  # Map polymers (20 -> base set, PA kept distinct)
  # ---------------------------------------------------------------------------
  polymerMap <- toolGetMapping("polymermappingRen2025.csv", type = "sectoral", where = "mrmfa")
  x <- toolAggregate(x, rel = polymerMap, dim = "polymer", from = "Source", to = "Target")

  # ---------------------------------------------------------------------------
  # Map sectors (9 -> 8 goods) per (sector, product, polymer), most specific
  # mapping row first; "all" is a wildcard.
  # ---------------------------------------------------------------------------
  sectorMap <- toolGetMapping("sectormappingRen2025.csv", type = "sectoral", where = "mrmfa")
  mapExact <- sectorMap %>%
    filter(.data$Product != "all", .data$Polymer != "all") %>%
    rename(t1 = "Target")
  mapProduct <- sectorMap %>%
    filter(.data$Product != "all", .data$Polymer == "all") %>%
    select(-"Polymer") %>%
    rename(t2 = "Target")
  mapSector <- sectorMap %>%
    filter(.data$Product == "all", .data$Polymer == "all") %>%
    select(-"Product", -"Polymer") %>%
    rename(t3 = "Target")

  df <- as.data.frame(x, rev = 3) %>%
    mutate(across(-".value", as.character))
  dims <- setdiff(names(df), ".value")
  df <- df %>%
    left_join(mapExact, by = c(sector = "Source", product = "Product", polymer = "Polymer")) %>%
    left_join(mapProduct, by = c(sector = "Source", product = "Product")) %>%
    left_join(mapSector, by = c(sector = "Source")) %>%
    mutate(sector = coalesce(.data$t1, .data$t2, .data$t3))
  if (anyNA(df$sector)) stop("Unmapped Ren sector(s) in sectormappingRen2025.csv")
  df <- df %>%
    group_by(across(all_of(dims))) %>%
    summarise(.value = sum(.data$.value), .groups = "drop")

  x <- as.magpie(as.data.frame(df), spatial = dims[1], temporal = dims[2], datacol = ".value")

  # ---------------------------------------------------------------------------
  # Rename disposal categories to the mrmfa vocabulary (the "/" placeholder stays)
  # ---------------------------------------------------------------------------
  disposalMap <- c(
    "/"            = "/",
    "Recycling"    = "Recycled",
    "Incineration" = "Incinerated",
    "Landfill"     = "Landfilled",
    "Untreated"    = "Untreated"
  )
  getItems(x, dim = "disposal") <- unname(disposalMap[getItems(x, dim = "disposal")])

  # ---------------------------------------------------------------------------
  # Complete the country set (all countries except China are 0)
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
