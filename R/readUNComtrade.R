#' Read cement and clinker trade data from UN Comtrade.
#'
#' United Nations Statistics Division. (n.d.). UN Comtrade Database.
#' Retrieved May 8, 2025, from https://comtradeplus.un.org/
#' @author Bennet Weiss.
#' @param subtype Character string specifying the scope
#'        - Imports
#'        - Exports
#' @param subset Character string specifying the stage of trade
#'        - cement
#'        - clinker
#'
readUNComtrade <- function(subtype, subset) {
  years <- c(1988:2023)
  trade <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[i]

    # get data
    name <- paste0("TradeData_2523_cement_", year, ".csv")
    path <- file.path("v2", name)
    data <- suppressMessages(suppressWarnings(read_csv(
      path,
      col_names = TRUE,
      col_select = c("reporterISO", "flowDesc", "cmdCode", "qtyUnitAbbr", "qty")
    )))

    if (!all(data$qtyUnitAbbr %in% c("kg", "N/A"))) {
      stop("All quantities should have unit 'kg'.")
    }

    # select relevant HS codes
    if (subset == "cement") {
      resources <- c(252321, 252329, 252330, 252390)
    } else if (subset == "clinker") {
      resources <- c(252310)
    } else {
      stop("Invalid subset. Choose either 'cement' or 'clinker'.")
    }
    data <- subset(data, data$cmdCode %in% resources)

    # select import / exports depending on subtype
    trade_type <- substr(subtype, 1, nchar(subtype) - 1)
    yearly_trade <- stats::aggregate(`qty` ~ `reporterISO`, data, sum, subset = data$flowDesc == trade_type)

    # compute net trade and store in list
    trade[[i]] <- data.frame(
      region = yearly_trade$reporterISO,
      time   = year,
      value  = yearly_trade$qty
    )
  }

  # stack trade together
  trade <- do.call(rbind, trade)

  x <- magclass::as.magpie(trade, spatial = 1, temporal = 2)
  getNames(x) <- NULL
  return(x)
}
