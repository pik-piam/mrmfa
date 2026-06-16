#' Returns the plastics sector splits from Ryberg et al. 2019,
#' mapping Ryberg sectors to remind-mfa sectors
#' @author Leonie Schweiger
calcPlSectorSplit <- function() {
  data_raw <- readSource("Ryberg", subtype = "Sector_split", convert = FALSE)
  use_map <- toolGetMapping("structuremappingRyberg.csv", type = "sectoral", where = "mrmfa")
  data <- toolAggregate(data_raw, rel = use_map, dim = 3, from = "Source", to = "Target")
  description <- paste(
    "Global plastic sector splits",
    "Data from Ryberg et al. 2019 doi: 10.1016/j.resconrec.2019.104459"
  )
  output <- list(
    x = data,
    weight = NULL,
    unit = "share",
    description = description,
    isocountries = FALSE,
    note = "dimensions: (Good,value)"
  )
  return(output)
}
