#' Read cement and clinker trade data from UN Comtrade. Get net trade (export).
#'
#' United Nations Statistics Division. (n.d.). UN Comtrade Database.
#' Retrieved May 8, 2025, from https://comtradeplus.un.org/
#' @author Bennet Weiss.
#' @param subtype Material subtype. Can be "cement" or "clinker"
readUNComtrade <- function(subtype) {
  years <- c(1988:1999, 2023, 2024)
  net_list <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[i]

    # get data
    name <- paste0("TradeData_2523_cement_", year, ".csv")
    path <- file.path("v1", name)
    data <- suppressMessages(suppressWarnings(readr::read_csv(
      path,
      col_names = TRUE,
      col_select = c("reporterISO", "flowDesc", "cmdCode", "qtyUnitAbbr", "qty")
    )))

    if (!all(data$qtyUnitAbbr %in% c("kg", "N/A"))) {
      stop("All quantities should have unit 'kg'.")
    }

    # select relevant HS codes
    if (subtype == "cement") {
      resources <- c(252321, 252329, 252330, 252390)
    } else if (subtype == "clinker") {
      resources <- c(252310)
    } else {
      stop("Invalid subtype. Choose either 'cement' or 'clinker'.")
    }
    data <- subset(data, data$cmdCode %in% resources)

    # sum imports and exports
    imports <- stats::aggregate(`qty` ~ `reporterISO`, data, sum, subset = data$flowDesc == "Import")
    exports <- stats::aggregate(`qty` ~ `reporterISO`, data, sum, subset = data$flowDesc == "Export")

    # rename for merge
    imports <- setNames(imports, c("region", "weight.imp"))
    exports <- setNames(exports, c("region", "weight.exp"))

    # merge, replace na with 0
    net_trade <- merge(imports, exports, by = "region", all = TRUE)
    net_trade[is.na(net_trade)] <- 0

    # compute net trade and store in list
    net_list[[i]] <- data.frame(
      region = net_trade$region,
      time   = year,
      value  = net_trade$weight.exp - net_trade$weight.imp
    )
  }

  # stack all net_trades together
  net_all <- do.call(rbind, net_list)

  x <- magclass::as.magpie(net_all, spatial = 1, temporal = 2)
  getNames(x) <- NULL
  return(x)
}
