#' Returns the carbon contents of different plastic types/polymers
#' @author Leonie Schweiger
calcPlCarbonContent <- function() {
  data_raw <- readSource("Plastics_CarbonContent", convert = FALSE)
  data <- add_dimension(data_raw, dim = 3.2, add = "Element", nm = "C")

  # calculate non-carbon contents
  other_elements <- 1 - data
  getNames(other_elements, dim = "Element") <- "Other Elements"

  data_final <- mbind(data, other_elements)

  description <- paste(
    "Carbon contents of plastic types. ",
    "Data from stochiometric calculations and rough estimates for broader categories"
  )
  output <- list(
    x = data_final,
    weight = NULL,
    unit = "ratio",
    description = description,
    isocountries = FALSE,
    note = "dimensions: (Material,Element,value)"
  )
  return(output)
}
