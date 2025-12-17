#' Calculate which product materials correspond to which Product application
#'
#' @author Bennet Weiss
#'
calcCeProductMaterialApplicationTransform <- function() {
  vals <- c(
    1, 1, 1, 1, 0, 0, 0, # concrete
    0, 0, 0, 0, 1, 1, 1 # mortar
  )

  dimnames <- list(
    c("concrete", "mortar"),
    c("C15", "C20", "C30", "C35", "finishing", "masonry", "maintenance")
  )

  arr <- array(
    vals,
    dim = c(2, 7),
    dimnames = dimnames
  )

  x <- as.magpie(arr)

  unit <- "N/A"
  description <- "Mapping of Product Material to Product Application."
  note <- "dimensions: (Product Material,Product Application,value)"

  output <- list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    note = note,
    isocountries = FALSE
  )
  return(output)
}
