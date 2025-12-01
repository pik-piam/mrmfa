#' Load Cullen 2012 fabrication yield
#' @description
#' Function to load fabrication yield data from Cullen et al. (2012).
#' Using the function \link{readCullen2012}.
#' @author Merlin Jo Hosak
calcStCullenFabricationYield <- function() {

  x <- readSource("Cullen2012", subtype = "giMatrix")
  x <- x[, , "End Use Goods - End Use Goods", pmatch = T]
  x <- x[, , c("Construction %", "Machinery %", "Products %", "Transport %")]

  getSets(x) <- c("region", "year", "parameter")
  getItems(x, dim = 3) <- c(
    "Construction Yield",
    "Machinery Yield",
    "Products Yield",
    "Transport Yield"
  )

  x <- list(
    x = x,
    weight = NULL,
    unit = 1,
    isocountries = FALSE,
    description = "Cullen 2012 Fabrication Yield",
    note        = "dimensions: (Good,value)"
  )

  return(x)
}
