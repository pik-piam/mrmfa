#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @export
calcStBIRmaxScrapShare <- function() {
  scrapShares <- readSource('BIR', convert=F)

  # Remove turkey as it is outlier
  scrapShares <- scrapShares[!getRegions(scrapShares) == "Turkey", , ]
  maxScrapShare <- magpply(X=scrapShares, MARGIN=3, FUN=function(x) stats::quantile(x, 0.95, na.rm = TRUE))

  final <- new.magpie()
  final[,] <- maxScrapShare[,]

  getSets(final) <- c("Region", "Year", "Parameter")
  getNames(final)<-'Max Scrap share'

  final <- list(x = final,
                weight = NULL,
                unit=1,
                description='Maximum scrap share in production')

  return(final)
}
