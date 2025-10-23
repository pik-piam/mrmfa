#' Read OECD GDP per capita data from 1500-2016
#' @author Merlin Jo Hosak
#' @export
readOECD_GDP <- function(subtype = 'gdppc') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'gdppc' = function() {
      x <- readxl::read_excel(path = './v1.1/GDPperCapita_Broad.xlsx',
                              range = 'A1:SY208')
      
      # delete duplicate rows where no data is available
      x <- x[-which(x$`country name` == 'Canada' & is.na(x$`ccode`)),]
      x <- x[-which(x$`country name` == 'Morocco' & is.na(x$`ccode`)),]
      x <- x[-which(x$`country name` == 'Sudan' & !is.na(x$`ccode`)),]
      # remove ccode column
      x$ccode <- NULL
      x <- as.magpie(x, spatial="country name")
      return(x)
    },
    
    NULL)
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

