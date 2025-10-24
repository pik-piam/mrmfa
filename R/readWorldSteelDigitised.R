#' Read Data from World Steel Association 1978-2022 yearbooks digitized to Excel sheets
#' E.g. from 1982: https://worldsteel.org/wp-content/uploads/Steel-Statistical-Yearbook-1982.pdf
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @importFrom rlang .data .
#' @export
readWorldSteelDigitised <- function(subtype = 'world_production') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'world_production' = function() {
      x <- readxl::read_excel(path = paste0('./v1.0/production/',
                                               'world_production_1900-1979.xlsx'),
                                 range = 'A4:B84')
      x <- as.magpie(x)
      
      # convert from Mt to t
      # conversion needs to happen here, as return value of conversion expects values for all countries
      x <- x * 1e6
      
      getItems(x,dim=3) <- 'value'
      
      return(x)
    },
    
    'production' = function() {
      filenames <- paste0(c('production_70s',
                            'production_80s',
                            'production_90s',
                            'production_00s'),
                          '.xlsx')
      production <- toolLoadWorldSteelDigitised(filenames, type='production')
      return(production)
    },
    
    'imports' = function() {
      filenames <- paste0(c('imports_70s',
                            'imports_80s',
                            'imports_90s',
                            'imports_00s'),
                          '.xlsx')
      imports <- toolLoadWorldSteelDigitised(filenames, type='trade')
      return(imports)
    },
    
    'exports' = function() {
      filenames <- paste0(c('exports_70s',
                            'exports_80s',
                            'exports_90s',
                            'exports_00s'),
                          '.xlsx')
      exports <- toolLoadWorldSteelDigitised(filenames, type='trade')
      return(exports)
    },
    
    'scrap_imports' = function() {
      filenames <- paste0(c('scrap_imports_70s',
                            'scrap_imports_80s',
                            'scrap_imports_90s',
                            'scrap_imports_00s'),
                          '.xlsx')
      scrap_imports <- toolLoadWorldSteelDigitised(filenames, type='scrap_trade')
      return(scrap_imports)
    },
    
    'scrap_exports' = function() {
      filenames <- paste0(c('scrap_exports_70s',
                            'scrap_exports_80s',
                            'scrap_exports_90s',
                            'scrap_exports_00s'),
                          '.xlsx')
      scrap_exports <- toolLoadWorldSteelDigitised(filenames, type='scrap_trade')
      return(scrap_exports)
    },
    
    'scrap_consumption' = function() {
      filenames <- paste0(c('scrap_consumption_75s',
                            'scrap_consumption_80s',
                            'scrap_consumption_85s',
                            'scrap_consumption_90s'),
                          '.xlsx')
      scrap_consumption <- toolLoadWorldSteelDigitised(filenames, type='scrap_consumption')
      return(scrap_consumption)
    },
    
    'indirect_imports_by_category_2013' = function() {
      x <- toolLoadIndirectTrade2013('indirect_imports')
      return(x)
    },
    
    'indirect_exports_by_category_2013' = function() {
      x <- toolLoadIndirectTrade2013('indirect_exports')
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

# ---- Functions ----

toolLoadWorldSteelDigitised <- function(filenames,type,version='1.0') {
  paths <- paste0('v', version, '/', type, '/', filenames)
  decades <- comprehenr::to_list(for(path in paths) toolWSDecadeRead(path))
  x <- toolWSDecadeMerge(decades)
  return(x)
}

toolLoadIndirectTrade2013 <- function(subtype) {
  x <- readxl::read_excel(path = paste0('./v1.0/indirect_trade_2013/',
                                        'WSA_', subtype,'_categories_2013.xlsx'))
  # delete unnecessary rows (total or other in the name or NA)
  x <- x %>%
    filter(!grepl("total|other", .[[1]], ignore.case = TRUE))
  x <- x[!is.na(x$country_name), ]
  
  x <- as.magpie(x,spatial='country_name')
  
  x <- add_columns(x,addnm=c("Construction", "Machinery", "Transport", "Products", "Total"),dim='variable')
  
  x[, ,'Construction'] <- 0
  x[,,'Machinery'] <- x[,,'Mechanical Machinery']
  x[,,'Transport'] <- x[,,'Automotive'] + x[,,'Other transport']
  x[,,'Products'] <- x[,,'Electrical Equipment'] + x[,,'Metal products'] + x[,,'Domestic appliances']
  x[,,'Total'] <- x[,,'Machinery'] + x[,,'Transport'] + x[,,'Products']
  
  # calc shares
  x[,,'Machinery'] <- x[,,'Machinery'] / x[,,'Total']
  x[,,'Transport'] <- x[,,'Transport'] / x[,,'Total']
  x[,,'Products'] <- x[,,'Products'] / x[,,'Total']
  
  # drop unnecessary columns
  x <- x[, , c('Construction', 'Machinery', 'Products', 'Transport')]
  
  countries <- getItems(x, dim=1)
  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"))$IgnoredRegions
  getItems(x, dim=1) <- toolCountry2isocode(countries,ignoreCountries = ignore)
  
  # remove rows with NA in country_name column
  x <- x[!is.na(getItems(x, dim=1)), ]
  
  return(x)
}

toolWSDecadeRead <- function(name) {
  x <- readxl::read_excel(path = name)
  
  # delete unnecessary rows (total or other in the name or NA)
  x <- x %>%
    filter(!grepl("total|other", .[[1]], ignore.case = TRUE))
  x <- x[!is.na(x$country_name), ]
  
  # convert to magpie
  x <- as.magpie(x, spatial=colnames(x)[1])

  # change to ISO country codes
  countries <- getItems(x,dim=1)
  countries <- gsub('_', '.', countries)  # replace underscores with dots as magclass sometimes does the opposite
  
  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"))$IgnoredRegions
  getItems(x, dim=1) <- toolCountry2isocode(countries,ignoreCountries = ignore)
  
  # remove new rows with NA in country_name column (that were ignored)
  x <- x[!is.na(getItems(x, dim=1)), ]
  
  return(x)
}

toolWSDecadeMerge <- function(magpies) {
  # get merged countries & years
  countries <- character(0)
  years <- character(0)
  for (magpie in magpies) {
    countries <- union(countries, getItems(magpie, dim=1))
    years <- union(years, getItems(magpie, dim=2))
  }
  
  # sort the indices
  countries <- sort(countries)
  years <- sort(years)
  
  # create a new magpie object with appropriate size
  x <- new.magpie(
    cells_and_regions = countries,
    years = years,
    names = "value",
    fill = NA,
    sets = names(dimnames(magpies[[1]]))
  )
  
  # fill in the data
  for (magpie in magpies) {
    x[getItems(magpie, dim=1), getItems(magpie, dim=2)] <- magpie
  }
  
  return(x)
}

