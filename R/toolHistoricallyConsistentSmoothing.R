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
#' scenario-specific spline at its end, yielding a C2-continuous transition.
#'
#' @param x A magpie object with a 'scenario' dimension.
#' @param lastHistYear Integer or NULL. Last year considered historical. If
#' NULL (default), it is detected automatically as the last year up to which
#' all scenarios in \code{x} share identical values. If given, the historical
#' period is forced up to this year and \code{refScenario} values are written
#' there even where scenarios originally differ. Ignored if x contains a
#' single scenario.
#' @param refScenario Character. Name of the scenario whose smoothed values
#' define the shared history (default "SSP2"). Must be located in 'scenario'
#' dimension of \code{x}. Ignored if x contains a single scenario.
#' @param fadeLength Integer > 0. Length of the transition window in years.
#' 1 produces a hard splice, which is generally discontinuous.
#' @param dof Degrees of freedom passed to \link[madrat]{toolTimeSpline}.
#' @param verbose Logical. If TRUE, print informational messages (e.g. the
#' automatically detected last historical year). Default FALSE.
#' @param ... Further arguments (e.g. peggedYears, anchorFactor) passed to
#' \link[madrat]{toolTimeSpline}.
#' @return Smoothed magpie object with scenario-independent history.
#' @author Bennet Weiss
toolHistoricallyConsistentSmoothing <- function(
  x,
  lastHistYear = NULL,
  refScenario = "SSP2",
  fadeLength = 10,
  dof = 8,
  verbose = FALSE,
  ...
) {
  if (!is.numeric(fadeLength) || length(fadeLength) != 1 || fadeLength <= 0) {
    stop("fadeLength must be a single positive number (larger than 0).")
  }

  smoothed <- toolTimeSpline(x, dof = dof, ...)

  # without the scenario dimension or with a single scenario there is nothing to harmonize
  if (!"scenario" %in% getSets(x)) {
    warning("No dimension 'scenario' found in x. Returning plainly smoothed object.")
    return(smoothed)
  }
  scens <- getItems(x, dim = "scenario")
  if (length(scens) <= 1) {
    return(smoothed)
  }

  if (!refScenario %in% scens) {
    stop(
      "refScenario '", refScenario, "' not found in dimension 'scenario'. Available: ", paste(scens, collapse = ", ")
    )
  }

  # selector for a single scenario in the scenario (sub)dimension
  sel_refScen <- list(scenario = refScenario)
  otherScens <- setdiff(scens, refScenario)
  years <- getYears(smoothed, as.integer = TRUE)

  if (is.null(lastHistYear)) {
    # detect the historical period as the leading run of years for which all
    # scenarios in the (unsmoothed) input share identical values
    refArr <- x[, , sel_refScen]
    otherArrs <- x[, , list(scenario = otherScens)]
    for (i in seq_along(years)) {
      sameThisYear <- all(refArr[, i, ] == otherArrs[, i, ])
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

  if (min(years) > lastHistYear) {
    warning("No years <= lastHistYear (", lastHistYear, ") in data. Returning plainly smoothed object.")
    return(smoothed)
  }
  if (lastHistYear >= max(years)) {
    warning("lastHistYear >= last data year. All scenarios are replaced by refScenario values.")
  }

  # prepare the fade weights: use C2-continuous smootherstep polynomial (see description) to transition from 0 to 1
  tNorm <- pmax(0, pmin(1, (years - lastHistYear) / fadeLength))
  w <- new.magpie(years = years, fill = (6 * tNorm^5 - 15 * tNorm^4 + 10 * tNorm^3))

  for (s in otherScens) {
    # overwrite hist period with refScen, then fade from reference to scenario-specific spline
    smoothed[, , s] <- (1 - w) * smoothed[, , sel_refScen] + w * smoothed[, , s]
  }
  return(smoothed)
}
