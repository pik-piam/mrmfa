#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @export
readCooper2014 <- function(subtype = 'lifetimes') {
  
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'lifetimes' = function() {
      
      path <- './v1.0/Cooper_2014_Lifetimes.xlsx'
      df <- readxl::read_excel(path,
                               sheet='Data')
      df <- as.array(as.matrix(df[,-1]))
      rownames(df) <- c('Construction',
                        'Machinery',
                        'Products',
                        'Transport')
      
      
      final <- as.magpie(df)
      getSets(final) <- c('region', 'year', 'parameter')
      
      final <- list(x = final, 
                    weight = NULL,
                    unit=1,
                    description='Cooper 2014 Lifetimes')
      return(final)
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
