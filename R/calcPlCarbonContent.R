#' Returns the carbon contents of different plastic types/polymers or HVCs
#' @param subtype either plastics or HVC
#' @author Leonie Schweiger
calcPlCarbonContent <- function(subtype = "plastics") {
  data_raw <- readSource("Plastics_CarbonContent", subtype = subtype, convert = FALSE)
  data <- add_dimension(data_raw, dim = 3.2, add = "Element", nm = "C")

  # calculate non-carbon contents
  other_elements <- 1 - data
  getNames(other_elements, dim = "Element") <- "Other Elements"

  data_final <- mbind(data, other_elements)

  if(subtype=="plastics"){
    description <- paste(
      "Carbon contents of plastic types. ",
      "Data from stochiometric calculations and rough estimates for broader categories"
    )
  }else if(subtype=="HVC"){
    description <- "Carbon contents of HVC. Data from stochiometric calculations."
  }

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
