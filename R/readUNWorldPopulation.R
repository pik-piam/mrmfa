readUNWorldPopulation <- function() {
  file <- "UN_World_Population_1900-2150.csv"
  x <- utils::read.csv(file)
  x <- as.magpie(x)
  x <- x * 1e9 # Convert from billions to individuals
  getItems(x, dim = 3) <- "value"
  return(x)
}
