#' Returns cumulative cement production casted to given dimensions.
#' Can be used as weight in cement-related aggregation without time component.
#' Assumes time dimension is in 2nd position.
#'
#' @author Bennet Weiss
#' @param castto magpie object that provides the desired dimensions.
#' Defaults to NULL, in which case no dimension casting is performed.
toolCeCumulativeCementProduction <- function(castto = NULL) {
  cement_production <- calcOutput("CeBinderProduction", subtype = "cement", aggregate = FALSE)
  cumulative_cement_production <- dimSums(cement_production, dim = 2)
  if (!is.null(castto)) {
    cumulative_cement_production <- magpie_expand(cumulative_cement_production, castto)
  }
  return(cumulative_cement_production)
}
