#' Convert data from WorldSteelAssociation Database
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDatabase <- function(x, subtype="production") {
  # convert from kt to t
  x <- x * 1e3
  
  # replace country names with ISO codes
  countries <- getItems(x, dim=1)
  countries <- gsub('_', '.', countries)  # replace _ with . for isocode conversion
  getItems(x, dim=1) <- toolCountry2isocode(countries)
  
  # Add historical mapping for Yugoslavia with last year being 2005
  # instead of 1991 as there is some aggregated data in this dataset
  # for the years 2002-2005 for Yugoslavia. Same for Belgium and Luxembourg.
  
  # TODO create toolISOHistoricalSpecific function that takes madrat mapping and changes values
  historical_mapping <- list(fromISO = c(rep('YUG', 6), rep('BLX',2)), 
                             toISO = c('SRB', 'MNE', 'SVN', 'HRV', 'MKD', 'BIH', 'BEL', 'LUX'),
                             lastYear = c(rep('y2005', 6), rep('y2003', 2))) %>% as.data.frame()
  
  x <- toolISOhistorical(x, overwrite=TRUE, mapping=historical_mapping)
  y <- toolISOhistorical(x)
  
  # remove rows with NA in country_name column
  x <- x[!is.na(getItems(x, dim=1)), ]
  
  x <- toolCountryFill(x, verbosity=2)
  
  return(x)
}
