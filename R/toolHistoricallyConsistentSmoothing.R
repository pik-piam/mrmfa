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
#' @param lastHistYear Integer or NULL. Last year considered historical. If
#' NULL (default), it is detected automatically as the last year up to which
#' all scenarios in \code{x} share identical values. If given, the historical
#' period is forced up to this year and \code{refScenario} values are written
#' there even where scenarios originally differ. Ignored if x contains a
#' single scenario.
#' @param refScenario Character. Name of the scenario whose smoothed values
#' define the shared history (default "SSP2").
#' @param scenarioDim Character. Name of the (sub)dimension holding the
#' scenarios (default "scenario"). If x has no such dimension, the object is
#' smoothed as a whole without any harmonization.
#' @param fadeLength Integer >= 0. Length of the transition window in years.
#' 0 produces a hard splice, which is generally discontinuous.
#' @param dof Degrees of freedom passed to \link[madrat]{toolTimeSpline}.
#' @param verbose Logical. If TRUE, print informational messages (e.g. the
#' automatically detected last historical year). Default FALSE.
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
                                                verbose = FALSE,
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

  if (!refScenario %in% scens) {
    stop(
      "refScenario '", refScenario, "' not found in dimension '", scenarioDim,
      "'. Available: ", paste(scens, collapse = ", ")
    )
  }

  # selector for a single scenario in the scenario (sub)dimension
  selScen <- function(s) stats::setNames(list(s), scenarioDim)
  selRef <- selScen(refScenario)
  otherScens <- setdiff(scens, refScenario)
  years <- getYears(smoothed, as.integer = TRUE)

  if (is.null(lastHistYear)) {
    # detect the historical period as the leading run of years for which all
    # scenarios in the (unsmoothed) input share identical values
    refArr <- as.array(x[, , selRef])
    otherArrs <- lapply(otherScens, function(s) as.array(x[, , selScen(s)]))
    for (i in seq_along(years)) {
      sameThisYear <- all(vapply(otherArrs, function(a) {
        isTRUE(all.equal(a[, i, ], refArr[, i, ], check.attributes = FALSE))
      }, logical(1)))
      if (!sameThisYear) break
      lastHistYear <- years[i]
    }
    if (is.null(lastHistYear)) {
      warning(
        "Scenarios already differ in the first year; no common historical ",
        "period could be detected. Returning plainly smoothed object."
      )
      return(smoothed)
    }
    if (verbose) message("Detected last historical year: ", lastHistYear)
  }

  if (!any(years <= lastHistYear)) {
    warning("No years <= lastHistYear (", lastHistYear, ") in data. Returning plainly smoothed object.")
    return(smoothed)
  }
  if (lastHistYear >= max(years)) {
    warning("lastHistYear >= last data year. All scenarios are replaced by refScenario values.")
  }

  histYears <- years[years <= lastHistYear]
  fadeYears <- years[years > lastHistYear & years <= lastHistYear + fadeLength]

  ref <- smoothed[, , selRef]
  refFade <- as.array(ref[, fadeYears, ])
  # prepare the fade weights
  tNorm <- (fadeYears - lastHistYear) / fadeLength
  w <- 6 * tNorm^5 - 15 * tNorm^4 + 10 * tNorm^3

  for (s in otherScens) {
    sel <- selScen(s)
    # overwrite history with the shared reference trajectory
    smoothed[, histYears, sel] <- ref[, histYears, ]
    # fade from reference to scenario-specific spline (w broadcast over region/data dims)
    sFade <- as.array(smoothed[, fadeYears, sel])
    smoothed[, fadeYears, sel] <- sweep(refFade, 2, 1 - w, `*`) + sweep(sFade, 2, w, `*`)
  }
  return(smoothed)
}
