#' Calc steel sector splits
#' @description
#' Function to load steel sector splits data in the four main end-use sectors
#' (construction, machinery, products, transport). Currently summarized data
#' from Pauliuk et al. (2013) is used that is only available for the US in
#' 2004, India in 1995-1999 and the UK in 1960-65, 1970, 1974-79. These are
#' based on the American Iron and Steel Institute, Spark Steel & Economy
#' Research, the Iron and Steel Statistics Bureau and Dahlström et al. (2004)
#' (see Metadata in File). See \link{readPauliuk2013} for
#' preprocessing.
#' @param subtype Sector split source, currently using Pauliuk  et al. (2013).
#' Other options is to use Pauliuk et al.'s stock sector splits or other
#' MFAs.
#' @author Merlin Jo Hosak
calcStSectorSplits <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----

  x <- readSource("Pauliuk2013")
  
  
  
  # if subtype is "low", extract rows where data is "India 1995-1999"
  if (subtype == "low") {
    x <- mselect(x, Parameter="India 1995-1999", collapseNames=TRUE)
  }
  else if (subtype == "high") {
    x <- mselect(x, Parameter="USA 2004", collapseNames=TRUE)
  }
  else {
    stop("Invalid subtype -- supported subtypes are high and low")
  }

  # remove .data column
  # x$Data1 <- NULL
  print(x)
  
    
  final <- list(
    x = as.magpie(x),
    weight = NULL,
    unit = 1,
    isocountries = FALSE,
    description = "Pauliuk 2013 steel sector splits", 
    note = "dimensions: (Good,value)"
  )
  
  return(final)

}
