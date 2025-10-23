#' Read cement and clinker trade data from Chatham House. Get net trade (export).
#'
#' Chatham House (2024), ‘resourcetrade.earth’, https://resourcetrade.earth/.
#' Accessed: 29.04.2025.
#' Data based on UN Comtrade.
#' @author Bennet Weiss.
#' @param subtype Material subtype. Can be "cement" or "clinker"
readChathamHouse <- function(subtype) {
  years <- 2000:2022
  net_list <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[i]

    # get data
    name <- paste0("resourcetradeearth-all-all-143-", year, ".xlsx")
    path <- file.path("v1", name)
    data <- readxl::read_xlsx(path, sheet = "Trades")

    # select data of interest
    data <- subset(data, data$Year == year) # each excel sheet contains up to 5 years
    if (subtype == "cement") {
      resources <- c(
        "Portland cement, other than white cement",
        "Portland cement, white or white artificially coloured",
        "Other hydraulic cements, except Portland or aluminous",
        "Aluminous cement"
      )
    } else if (subtype == "clinker") {
      resources <- c("Cement clinkers")
    } else {
      stop("Invalid subtype. Choose either 'cement' or 'clinker'.")
    }
    data <- subset(data, data$Resource %in% resources)

    # sum imports and exports
    imports <- stats::aggregate(`Weight (1000kg)` ~ `Importer ISO3`, data, sum)
    exports <- stats::aggregate(`Weight (1000kg)` ~ `Exporter ISO3`, data, sum)

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
