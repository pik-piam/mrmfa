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
#' If \code{lastSmoothedYear} is given, only the years up to it are smoothed,
#' e.g. to exclude the low-quality extension of the SSP drivers beyond 2100.
#' Splicing the later years unchanged to the smoothed values would leave a
#' jump in level and slope at \code{lastSmoothedYear}, since the spline does not
#' pass through the data. Therefore, the later years are corrected by the
#' deviation of the unsmoothed from the smoothed level and slope in
#' \code{lastSmoothedYear}, faded out over \code{joinFadeLength} years with the
#' same smootherstep polynomial. The result continues the smoothed trajectory
#' and returns to the unsmoothed data after the fade window. Values up to
#' \code{lastSmoothedYear} do not depend on the later years.
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
#' @param lastSmoothedYear Integer or NULL. Last year that is smoothed; later
#' years are joined continuously. If NULL (default), all
#' years are smoothed.
#' @param joinFadeLength Integer > 0. Number of years after
#' \code{lastSmoothedYear} over which the later years return to the unsmoothed
#' data. Ignored if \code{lastSmoothedYear} is NULL.
#' @param verbose Logical. If TRUE, print informational messages (e.g. the
#' automatically detected last historical year). Default FALSE.
#' @param ... Further arguments (e.g. peggedYears, anchorFactor) passed to
#' \link[madrat]{toolTimeSpline}.
#' @return Smoothed magpie object with scenario-independent history.
#' @author Bennet Weiss, Tobias Diez
toolHistoricallyConsistentSmoothing <- function(
  x,
  lastHistYear = NULL,
  refScenario = "SSP2",
  fadeLength = 10,
  dof = 8,
  lastSmoothedYear = NULL,
  joinFadeLength = 20,
  verbose = FALSE,
  ...
) {
  checkFadeLength <- function(value, name) {
    if (!isTRUE(all(is.numeric(value), length(value) == 1, value > 0))) {
      stop(name, " must be a single positive number (larger than 0).")
    }
  }

  # C2-continuous transition from 0 to 1 on [0, 1]
  smootherstep <- function(t) {
    6 * t^5 - 15 * t^4 + 10 * t^3
  }

  # replace the years after smoothedYears in smoothed by the unsmoothed data, corrected by the
  # deviation of smoothed from unsmoothed level and slope (backward difference) in the last smoothed year
  joinContinuously <- function(smoothed, raw, smoothedYears, fadeLength) {
    years <- getYears(raw, as.integer = TRUE)
    laterYears <- years[years > max(smoothedYears)]
    if (length(laterYears) == 0) {
      return(smoothed)
    }

    joinYears <- utils::tail(smoothedYears, 2)
    levelCorrection <- setYears(smoothed[, joinYears[2], ] - raw[, joinYears[2], ], NULL)
    slopeCorrection <- setYears(
      (smoothed[, joinYears[2], ] - smoothed[, joinYears[1], ]) - (raw[, joinYears[2], ] - raw[, joinYears[1], ]),
      NULL
    ) / (joinYears[2] - joinYears[1])

    elapsed <- laterYears - joinYears[2]
    elapsedYears <- new.magpie(years = laterYears, fill = elapsed)
    remaining <- new.magpie(years = laterYears, fill = 1 - smootherstep(pmin(1, elapsed / fadeLength)))
    smoothed[, laterYears, ] <- raw[, laterYears, ] + (levelCorrection + slopeCorrection * elapsedYears) * remaining
    return(smoothed)
  }

  checkFadeLength(fadeLength, "fadeLength")

  if (!is.null(lastSmoothedYear)) {
    checkFadeLength(joinFadeLength, "joinFadeLength")
    years <- getYears(x, as.integer = TRUE)
    smoothedYears <- years[years <= lastSmoothedYear]
    if (length(smoothedYears) < 2) {
      stop("At least two years up to lastSmoothedYear (", lastSmoothedYear, ") are needed for smoothing.")
    }

    smoothed <- x
    smoothed[, smoothedYears, ] <- toolHistoricallyConsistentSmoothing(
      x[, smoothedYears, ],
      lastHistYear = lastHistYear, refScenario = refScenario, fadeLength = fadeLength, dof = dof,
      verbose = verbose, ...
    )
    return(joinContinuously(smoothed, x, smoothedYears, joinFadeLength))
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
  w <- new.magpie(years = years, fill = smootherstep(tNorm))

  for (s in otherScens) {
    # overwrite hist period with refScen, then fade from reference to scenario-specific spline
    smoothed[, , s] <- (1 - w) * smoothed[, , sel_refScen] + w * smoothed[, , s]
  }
  return(smoothed)
}
