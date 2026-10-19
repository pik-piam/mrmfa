#' Calculate share of the residential stock that is not permanently occupied
#'
#' Includes vacant dwellings and second (seasonal/holiday) homes. Used to scale up
#' primary (permanently occupied) residential floorspace to the total built stock.
#' Data priority: Odyssee (total minus permanently occupied
#' dwellings), OECD Affordable Housing Database (vacant plus holiday homes),
#' floorspace-weighted average for all remaining countries, manual value for China.
#'
#' @author Bennet Weiss
calcCeUnoccupiedResidentialStockShare <- function() {
  floorspace <- calcOutput("CeFloorspaceEDGEB", aggregate = FALSE, smooth = TRUE)[, 2023, "Res"]
  weight <- collapseDim(floorspace, dim = c(2, 3))

  # Odyssee: share of dwellings that are not permanently occupied, latest available year since 2000
  odyssee <- readSource("Odyssee", subtype = "260305")
  odyssee <- collapseDim(odyssee[, getYears(odyssee, as.integer = TRUE) >= 2000, c("nbrlog", "nbrlpr")])
  share_odyssee_years <- 1 - odyssee[, , "nbrlpr"] / odyssee[, , "nbrlog"]
  # some countries (e.g. LTU) mostly report total stock as permanently occupied stock
  share_odyssee_years[share_odyssee_years < 0.01] <- NA
  # fill from the latest year backwards so each country gets its latest available value
  years <- rev(getYears(share_odyssee_years))
  share <- collapseDim(share_odyssee_years[, years[1], ])
  for (year in years[-1]) {
    missing <- is.na(share)
    share[missing] <- share_odyssee_years[, year, ][missing]
  }

  # OECD: vacant dwellings plus seasonal/holiday homes
  share_oecd <- collapseDim(readSource("OECDAffordableHousingDatabase", subtype = "both") / 100)
  share[is.na(share)] <- share_oecd[is.na(share)]

  # set rest of world to floorspace-weighted average of available data
  share[is.na(share)] <- dimSums(share * weight, dim = 1, na.rm = TRUE) / sum(weight[!is.na(share)])

  # set China manually. The urban unused rate of 17.4% from Zheng et al. (2025) as lower bound.
  # Zheng, H., Zhang, R., Yin, X. et al.
  # Unused housing in urban China and its carbon emission impact.
  # Nat Commun 16, 1985 (2025). https://doi.org/10.1038/s41467-025-57217-7
  # It only counts dwellings sold at least two years earlier and never occupied, in 56 major cities.
  # It excludes developer inventory, previously occupied vacant dwellings, second homes, smaller
  # cities and rural housing. We use an estimate of 20%
  share["CHN", ] <- 0.2

  unit <- "ratio"
  description <- paste0(
    c(
      "Share of residential stock that is not permanently occupied (vacant dwellings and second homes).",
      "Europe based on:",
      "Odyssee database, total stock of dwellings minus stock of permanently occupied dwellings,",
      "https://odyssee.enerdata.net/database/",
      "Other OECD countries based on:",
      "OECD (2024), OECD Affordable Housing Database - indicator HM1.1. Housing stock and construction,",
      "https://oe.cd/ahd",
      "Rest of world (except China): floorspace-weighted average of the above.",
      "China: 20%, set manually based on urban and rural estimates:",
      "Zheng, H., Zhang, R., Yin, X. et al.",
      "Unused housing in urban China and its carbon emission impact.",
      "Nat Commun 16, 1985 (2025). https://doi.org/10.1038/s41467-025-57217-7",
      "(17.4% unused in 56 major cities, lower bound for whole China).",
    ),
    collapse = "\n"
  )
  note <- "dimensions: (Region,value)"

  output <- list(
    x = share,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 0,
    max = 1
  )
  return(output)
}
