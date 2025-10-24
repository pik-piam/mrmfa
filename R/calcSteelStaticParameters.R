#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @export
calcSteelStaticParameters <- function() {
  cullen <- calcOutput('CullenStaticParameters', aggregate=F)
  scrapInBOFRate <- readSource('WorldSteelParameters', subtype='scrapInBOFRate')
  maxScrapShare <- calcOutput('BIRmaxScrapShare', aggregate=F)
  
  final <- mbind(cullen, scrapInBOFRate, maxScrapShare)
  
  final <- list(x = final, 
                weight = NULL,
                unit=1,
                description='Steel static parameters')
  return(final)
}
