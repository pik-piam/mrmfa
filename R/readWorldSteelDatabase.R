#' Read Data from World Steel Association online Database,
#' downloaded to Excel files in the ./v1.0 directory. 
#' They should be updated yearly via the PIK's subscription to the database.
#' Most datasets are available between around 2002 and 2022 
#' on a yearly resolution.
#' @author Merlin Jo Hosak
#' @export
readWorldSteelDatabase <- function(subtype = 'production') {
  readWSDatabaseStandard <- function(name, version='1.0') {
    # read data from Excel file
    path <- paste0('./v',version,'/', name,'.xlsx')
    
    # Read the excel and suppress warning about non-numeric values in B column 
    # as WS files are always in the same format, last rows will be cut anyways.
    x <- readxl::read_excel(path, skip=2) %>% suppressSpecificWarnings(
      regularExpr = "Expecting numeric in B")  
    
    # delete last 5 rows as they are irrelevant in WS Database files
    x <- x[1:(nrow(x)-5), ]
    
    
    # Delete Others and World rows
    x <- x[!x$Country %in% c('Others', 'World'), ]
    
    # convert to magpie object
    x <- as.magpie(x, spatial="Country")
    
    return(x)
  }
  
  readWSDatabaseIndirectTrade <- function(filename){
    x <- readWSDatabaseStandard(filename)
    # distribute Belgium Luxemburg 80/20 %
    
    # add new country row for country bellux
    x <- add_columns(x,addnm=c('Belgium', 'Luxemburg', 'Serbia', 'Montenegro'),dim=1)
    
    
    x['Belgium',] <- x['Belgium-Luxemburg',] * 0.8
    x['Luxemburg',] <- x['Belgium-Luxemburg',] * 0.2
    x <- x[-which(rownames(x) == 'Belgium-Luxemburg'),]
    
    x['Serbia',] <- x['Serbia-Montenegro',] * 0.9
    x['Montenegro',] <- x['Serbia-Montenegro',] * 0.1
    x <- x[-which(rownames(x) == 'Serbia-Montenegro'),]
  }
  
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'production' = function() {
      x <- readWSDatabaseStandard('P01_crude_2023-10-23')
      return(x)
    },
    
    'imports' = function() {
      x <- readWSDatabaseStandard('T02_imports_finished-2023-10-23')
      return(x)
    },
    
    'exports' = function() {
      x <- readWSDatabaseStandard('T01_exports_finished-2023-10-23')
      return(x)
    },
    
    'scrap_imports' = function() {
      x <- readWSDatabaseStandard('T18_imports_scrap-2023-10-23')
      return(x)
    },
    
    'scrap_exports' = function() {
      x <- readWSDatabaseStandard('T17_exports_scrap-2023-10-23')
      return(x)
    },
    
    'indirect_imports' = function () {
      x <- readWSDatabaseIndirectTrade('I02_indirect_imports_2023-10-23')
      return(x)
    },
    
    'indirect_exports' = function () {
      x <- readWSDatabaseIndirectTrade('I01_indirect_exports_2023-10-23')
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
