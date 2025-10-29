#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @export
calcStStaticParameters <- function() {
  cullen <- calcOutput('StCullenStaticParameters', aggregate=F)
  scrapInBOFRate <- readSource('WorldSteelParameters', subtype='scrapInBOFRate')
  maxScrapShare <- calcOutput('StBIRmaxScrapShare', aggregate=F)
  
  final <- mbind(cullen, scrapInBOFRate, maxScrapShare)
  
  final <- list(x = final, 
                weight = NULL,
                unit=1,
                description='Steel static parameters')
  return(final)
}
