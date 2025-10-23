#' Calculates global clinker or cement trade (net import)
#' based on Chatham House, UN Comtrade, and USGS (in the case of cement).
#'
#' @author Bennet Weiss
#' @param subtype Material subtype. Can be "cement" or "clinker".
calcMCeMaterialTrade <- function(subtype) {
  # collect trade data
  trade_chatham <- readSource("ChathamHouse", subtype = subtype)
  trade_comtrade <- readSource("UNComtrade", subtype = subtype)
  trade <- magpiesort(mbind(trade_chatham, trade_comtrade))

  # add USGS trade data for US
  if (subtype == "cement") {
    trade_usgs <- readSource("USGSDS140")
    # avoid time overlap with trade
    trade_usgs_cut <- trade_usgs[, magclass::getYears(trade_usgs, as.integer = TRUE) <= 1987, ]
    trade <- magpiesort(mbind(trade, trade_usgs_cut))
    # add missing data for overlapping years manually
    trade["USA", c("y1988", "y1989", "y1990")] <- trade_usgs["USA", c("y1988", "y1989", "y1990")]
  }

  trade[is.na(trade)] <- 0

  # balance trade
  production <- calcOutput("MCeBinderProduction", subtype = subtype, aggregate = FALSE)[, getYears(trade)]
  total_production <- dimSums(production, dim = 1)
  trade_imbalance <- dimSums(trade, dim = 1)
  trade <- trade - trade_imbalance * production / total_production

  unit <- "tonnes (t)"
  description_general <- paste("Net trade (i.e. net export) of ", subtype, ". Data from:")
  description_chatham <- paste(
    "Chatham House (2024), 'resourcetrade.earth', https://resourcetrade.earth/",
    "Accessed: 29.04.2025. Data based on UN Comtrade."
  )
  description_comtrade <- paste(
    "United Nations Statistics Division. (n.d.). UN Comtrade Database.",
    "Retrieved May 8, 2025, from https://comtradeplus.un.org/"
  )
  description_usgs <- paste(
    "U.S. Geological Survey, 2020, Cement statistics, in Kelly, T.D., and Matos, G.R., comps.",
    "Historical statistics for mineral and material commodities in the United States:",
    "U.S. Geological Survey Data Series 140",
    "accessed 12.02.2025",
    "at https://www.usgs.gov/centers/national-minerals-information-center/",
    "historical-statistics-mineral-and-material-commodities."
  )
  if (subtype == "cement") {
    description <- paste(
      description_general, "\n",
      "1. For 1988 - 1999 and 2023-2024: ", description_chatham, "\n",
      "2. For 2000 - 2022: ", description_comtrade, "\n",
      "3. For USA, 1900 - 1990: ", description_usgs, "\n"
    )
  } else if (subtype == "clinker") {
    description <- paste(
      description_general, "\n",
      "1. For 1988 - 1999 and 2023-2024: ", description_chatham, "\n",
      "2. For 2000 - 2022: ", description_comtrade, "\n"
    )
  } else {
    stop("Invalid subtype. Choose either 'cement' or 'clinker'.")
  }
  note <- "dimensions: (Historic Time,Region,value)"
  output <- list(x = trade, weight = NULL, unit = unit, description = description, note = note)
  return(output)
}
