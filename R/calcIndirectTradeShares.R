#' Calculate indirect trade shares from World Steel Digitised data on indirect
#' trade in 2013.
#'
#' @author Falk Benke
#'
calcIndirectTradeShares <- function() {
  x <- readSource("WorldSteelDigitised", subtype = "indirectTrade")

  x <- add_columns(x, addnm = c("Construction", "Machinery", "Transport", "Products", "Total"), dim = "variable")

  # Construction is always zero
  allZero <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
  x[!allZero, , "Construction"] <- 0

  x[, , "Machinery"] <- x[, , "Mechanical Machinery"]
  x[, , "Transport"] <- x[, , "Automotive"] + x[, , "Other transport"]
  x[, , "Products"] <- x[, , "Electrical Equipment"] + x[, , "Metal products"] + x[, , "Domestic appliances"]
  x[, , "Total"] <- x[, , "Machinery"] + x[, , "Transport"] + x[, , "Products"]

  x[, , "Machinery"] <- x[, , "Machinery"] / x[, , "Total"]
  x[, , "Transport"] <- x[, , "Transport"] / x[, , "Total"]
  x[, , "Products"] <- x[, , "Products"] / x[, , "Total"]

  x <- x[, , c("Construction", "Machinery", "Products", "Transport")]

  return(list(
    x = x,
    weight = NULL,
    unit = "shares",
    description = glue::glue("Steel trade shares derived from World Steel \\
                             Digitised data on indirect trade in 2013.")
  ))
}
