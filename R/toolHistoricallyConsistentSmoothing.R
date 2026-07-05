#' Historically consistent time smoothing across scenarios
#'
#' @description
#' Smooths a magpie object over time using \link[madrat]{toolTimeSpline} while
#' ensuring that smoothed values in the historical period are identical across
#' all scenarios. Plain per-scenario spline smoothing lets scenario-specific
#' future data pull the fit inside the historical period, so smoothed
#' historical values would otherwise differ between scenarios.
#' After smoothing, all years up to \code{lastHistYear} are replaced in every
#' scenario by the smoothed values of \code{refScenario}. To avoid a
#' derivative discontinuity at the transition, each non-reference scenario is
#' faded from the reference trajectory to its own trajectory over
#' \code{fadeLength} years using the smootherstep polynomial
#' \eqn{w(t) = 6t^5 - 15t^4 + 10t^3}, whose first and second derivatives
#' vanish at both endpoints. The blended curve therefore matches the slope of
#' the shared history at the start of the fade window and the slope of the
#' scenario-specific spline at its end, yielding a (at least) C1-continuous,
#' in fact C2-continuous, transition.
#'
#' @param x A magpie object.
#' @param lastHistYear Integer. Last year considered historical. Required if
#' x contains multiple scenarios, ignored otherwise.
#' @param refScenario Character. Name of the scenario whose smoothed values
#' define the shared history (default "SSP2").
#' @param scenarioDim Character. Name of the (sub)dimension holding the
#' scenarios (default "scenario"). If x has no such dimension, the object is
#' smoothed as a whole without any harmonization.
#' @param fadeLength Integer >= 0. Length of the transition window in years.
#' 0 produces a hard splice, which is generally discontinuous.
#' @param dof Degrees of freedom passed to \link[madrat]{toolTimeSpline}.
#' @param ... Further arguments (e.g. peggedYears, anchorFactor) passed to
#' \link[madrat]{toolTimeSpline}.
#' @return Smoothed magpie object with scenario-independent history.
#' @author Bennet Weiss
toolHistoricallyConsistentSmoothing <- function(x,
                                                lastHistYear = NULL,
                                                refScenario = "SSP2",
                                                scenarioDim = "scenario",
                                                fadeLength = 10,
                                                dof = 8,
                                                ...) {
  if (!is.numeric(fadeLength) || length(fadeLength) != 1 || fadeLength < 0) {
    stop("fadeLength must be a single non-negative number.")
  }

  smoothed <- toolTimeSpline(x, dof = dof, ...)

  # without a scenario dimension or with a single scenario there is nothing to harmonize
  if (!scenarioDim %in% getSets(x)) {
    warning("No dimension '", scenarioDim, "' found in x. Returning plainly smoothed object.")
    return(smoothed)
  }
  scens <- getItems(x, dim = scenarioDim)
  if (length(scens) <= 1) {
    return(smoothed)
  }

  if (is.null(lastHistYear)) {
    stop("lastHistYear must be provided when x contains multiple scenarios.")
  }
  if (!refScenario %in% scens) {
    stop(
      "refScenario '", refScenario, "' not found in dimension '", scenarioDim,
      "'. Available: ", paste(scens, collapse = ", ")
    )
  }

  years <- getYears(smoothed, as.integer = TRUE)
  if (!any(years <= lastHistYear)) {
    warning("No years <= lastHistYear (", lastHistYear, ") in data. Returning plainly smoothed object.")
    return(smoothed)
  }
  if (lastHistYear >= max(years)) {
    warning("lastHistYear >= last data year. All scenarios are replaced by refScenario values.")
  }

  histYears <- years[years <= lastHistYear]
  fadeYears <- years[years > lastHistYear & years <= lastHistYear + fadeLength]

  selRef <- stats::setNames(list(refScenario), scenarioDim)
  ref <- smoothed[, , selRef]
  # prepare the fade weights
  tNorm <- (fadeYears - lastHistYear) / fadeLength
  w <- 6 * tNorm^5 - 15 * tNorm^4 + 10 * tNorm^3
  refArr <- as.array(ref[, fadeYears, ])
  for (s in setdiff(scens, refScenario)) {
    selS <- stats::setNames(list(s), scenarioDim)
    # replace history by shared reference values
    smoothed[, histYears, selS] <- ref[, histYears, ]
    sArr <- as.array(smoothed[, fadeYears, selS])
    # broadcast w across regions and data dims
    blended <- sweep(refArr, 2, 1 - w, `*`) + sweep(sArr, 2, w, `*`)
    smoothed[, fadeYears, selS] <- blended
  }
  return(smoothed)
}
