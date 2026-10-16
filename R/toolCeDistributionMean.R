#' Parameter names of a distribution, in the column order of the Cao2024 source data.
#'
#' @author Bennet Weiss
#' @param distribution Name of the distribution: "Weibull", "Uniform", "Triangular", "Normal" or "Point".
toolCeDistributionParameters <- function(distribution) {
  layouts <- list(
    Weibull = c("scale", "shape", "min", "max"),
    Uniform = c("max", "min"),
    Triangular = c("mode", "max", "min"),
    Normal = c("mean", "sd", "min", "max"),
    Point = "value"
  )
  if (!distribution %in% names(layouts)) {
    stop("Distribution ", distribution, " not implemented.")
  }
  return(layouts[[distribution]])
}

#' Calculate the mean of distributions given by their parameters.
#'
#' @author Bennet Weiss
#' @param x magpie object with a subdimension "parameter" holding the distribution parameters
#' (see \code{\link{toolCeDistributionParameters}}).
#' @param distribution Name of the distribution: "Weibull", "Uniform", "Triangular", "Normal" or "Point".
#' @param harmonic Bool if the harmonic mean (1 / E[1/X]) should be calculated instead of the mean.
#' Only implemented for "Weibull" and "Uniform".
#' @return magpie object without the "parameter" subdimension.
toolCeDistributionMean <- function(x, distribution, harmonic = FALSE) {
  if (!harmonic) {
    mean_functions <- list(
      Weibull = toolMeanTruncWeibull,
      Uniform = toolMeanUniform,
      Triangular = toolMeanTriangular,
      Normal = toolMeanTruncNorm,
      Point = function(parameters) as.numeric(parameters[[1]])
    )
  } else {
    mean_functions <- list(
      Weibull = toolFmeanTruncWeibull,
      Uniform = toolFmeanUniform
    )
  }
  if (!distribution %in% names(mean_functions)) {
    stop("Mean (harmonic = ", harmonic, ") not implemented for distribution ", distribution, ".")
  }

  parameters <- toolCeDistributionParameters(distribution)
  present <- getItems(x, dim = "parameter")
  if (!setequal(present, parameters)) {
    stop(
      "Parameters (", paste(present, collapse = ", "), ") do not match distribution ", distribution,
      " (", paste(parameters, collapse = ", "), ")."
    )
  }

  columns <- lapply(parameters, function(p) as.vector(mselect(x, parameter = p)))
  names(columns) <- parameters
  out <- collapseDim(mselect(x, parameter = parameters[[1]]), dim = "parameter")
  out[, , ] <- mean_functions[[distribution]](as.data.frame(columns))
  return(out)
}

#' Calculate the mean based on parameters from a truncated Weibull distribution.
#'
#' @author Bennet Weiss
#' @param parameters Array of the four parameters (scale, shape, min, max) of a Weibull distribution
toolMeanTruncWeibull <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)

  if (!is.data.frame(parameters) || ncol(parameters) != 4L) {
    stop("parameters must be a data.frame/matrix (4 columns) or a length-4 vector/list.")
  }

  scale <- as.numeric(parameters[[1]])
  shape <- as.numeric(parameters[[2]])
  a <- as.numeric(parameters[[3]])
  b <- as.numeric(parameters[[4]])

  # Validate per-row
  bad <- !is.finite(shape) | !is.finite(scale) | shape <= 0 | scale <= 0 |
    !is.finite(a) | !is.finite(b) | a < 0 | !(b > a)
  if (any(bad)) {
    stop(sprintf("Invalid parameter rows: %s", paste(which(bad), collapse = ", ")))
  }

  k <- shape
  lambda <- scale
  ua <- (a / lambda)^k
  ub <- (b / lambda)^k
  s <- 1 + 1 / k

  # numerator: lambda [gamma(s, ub) - gamma(s, ua)]  where gamma is lower incomplete gamma
  num <- lambda * gamma(s) * (stats::pgamma(ub, shape = s, rate = 1) - stats::pgamma(ua, shape = s, rate = 1))
  # denominator: probability mass of [a, b], i.e. the difference of the CDF at b and a
  den <- exp(-ua) - exp(-ub)
  return(num / den)
}

#' Calculate the mean based on parameters from a uniform distribution.
#'
#' @author Bennet Weiss
#' @param parameters Array of the two parameters (max, min) of a uniform distribution
toolMeanUniform <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)

  if (!is.data.frame(parameters) || ncol(parameters) != 2L) {
    stop("parameters must be a data.frame/matrix (2 columns) or a length-2 vector/list.")
  }

  b <- as.numeric(parameters[[1]])
  a <- as.numeric(parameters[[2]])

  # Validate per-row
  bad <- !is.finite(a) | !is.finite(b) | !(b > a)
  if (any(bad)) {
    stop(sprintf("Invalid parameter rows: %s", paste(which(bad), collapse = ", ")))
  }

  return((a + b) / 2)
}

#' Calculate the mean based on parameters from a triangular distribution.
#'
#' @author Bennet Weiss
#' @param parameters Array of the three parameters (mode, max, min) of a triangular distribution
toolMeanTriangular <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)
  if (!is.data.frame(parameters) || ncol(parameters) != 3L) {
    stop("parameters must be a data.frame/matrix (3 columns: mode,max,min) or a length-3 vector/list.")
  }
  mode <- as.numeric(parameters[[1]])
  b <- as.numeric(parameters[[2]]) # max
  a <- as.numeric(parameters[[3]]) # min

  bad <- !is.finite(a) | !is.finite(b) | !is.finite(mode) |
    !(b > a) | !(mode > a & mode < b)
  if (any(bad)) {
    stop(sprintf(
      "Invalid triangular parameter rows (need a < mode < b): %s",
      paste(which(bad), collapse = ", ")
    ))
  }

  (a + b + mode) / 3
}

#' Calculate the mean based on parameters from a truncated normal distribution.
#'
#' @author Bennet Weiss
#' @param parameters Array of the four parameters (mean, std, min, max) of a truncated normal distribution
toolMeanTruncNorm <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)
  if (!is.data.frame(parameters) || ncol(parameters) != 4L) {
    stop("parameters must be a data.frame/matrix (4 columns: mean, std, min, max) or a length-4 vector/list.")
  }

  mu <- as.numeric(parameters[[1]]) # mean of original distribution
  sigma <- as.numeric(parameters[[2]]) # std of original distribution
  a <- as.numeric(parameters[[3]]) # min (lower bound)
  b <- as.numeric(parameters[[4]]) # max (upper bound)

  bad <- !is.finite(a) | !is.finite(b) | !is.finite(mu) | !is.finite(sigma) |
    !(b > a) | !(sigma > 0)
  if (any(bad)) {
    stop(sprintf(
      "Invalid truncated normal parameters (need a < b and sigma > 0): %s",
      paste(which(bad), collapse = ", ")
    ))
  }

  alpha <- (a - mu) / sigma
  beta <- (b - mu) / sigma

  # Calculate the truncated normal mean
  # using the formula: mu + sigma * (pdf(alpha) - pdf(beta)) / (cdf(beta) - cdf(alpha))
  Z <- stats::pnorm(beta) - stats::pnorm(alpha)
  truncated_mean <- mu + sigma * (stats::dnorm(alpha) - stats::dnorm(beta)) / Z

  return(truncated_mean)
}


#' Conditional harmonic mean (1 / E[1/X]) for truncated Weibull on [a,b].
#' Parameters: (scale, shape, min, max). Requires (shape>1 or a>0). Diverges if a=0 & shape<=1.
#' Returns 1 / E[1/X | a<=X<=b].
#'
#' @author Bennet Weiss
#' @param parameters Array of the four parameters (scale, shape, min, max) of a Weibull distribution
toolFmeanTruncWeibull <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)
  if (!is.data.frame(parameters) || ncol(parameters) != 4L) {
    stop("parameters must be a data.frame/matrix (4 columns) or a length-4 vector/list.")
  }
  scale <- as.numeric(parameters[[1]])
  shape <- as.numeric(parameters[[2]])
  a <- as.numeric(parameters[[3]])
  b <- as.numeric(parameters[[4]])
  bad <- !is.finite(shape) | !is.finite(scale) | shape <= 0 | scale <= 0 |
    !is.finite(a) | !is.finite(b) | a < 0 | !(b > a)
  if (any(bad)) stop(sprintf("Invalid parameter rows: %s", paste(which(bad), collapse = ", ")))

  out <- numeric(length(shape))
  for (i in seq_along(shape)) {
    k <- shape[i]
    lambda <- scale[i]
    ai <- a[i]
    bi <- b[i]
    ua <- (ai / lambda)^k
    ub <- (bi / lambda)^k
    den <- exp(-ua) - exp(-ub) # probability mass of [a, b]
    if (ai == 0 && k <= 1) {
      stop(sprintf("Row %d: E[1/X] diverges (a=0, shape<=1).", i))
    }

    # Compute numerator of E[1/X] (call it numEInv); then harmonic mean = den / numEInv.
    if (k > 1) {
      s <- 1 - 1 / k
      numEInv <- (1 / lambda) * gamma(s) *
        (stats::pgamma(ub, shape = s, rate = 1) - stats::pgamma(ua, shape = s, rate = 1))
    } else {
      f_rec <- function(x) (k / lambda) * (x / lambda)^(k - 1) * exp(-(x / lambda)^k) / x
      numEInv <- stats::integrate(f_rec, lower = ai, upper = bi, rel.tol = 1e-8)$value
    }
    out[i] <- den / numEInv # inverse of the conditional E[1/X]
  }
  out
}

#' Conditional harmonic mean (1 / E[1/X]) for Uniform(a,b).
#' Parameters layout kept: col1 = b (max), col2 = a (min). Requires 0 < a < b.
#' Returns (b - a)/(log b - log a).
#'
#' @author Bennet Weiss
#' @param parameters Array of the two parameters (max, min) of a uniform distribution
toolFmeanUniform <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)
  if (!is.data.frame(parameters) || ncol(parameters) != 2L) {
    stop("parameters must be a data.frame/matrix (2 columns) or a length-2 vector/list.")
  }
  b <- as.numeric(parameters[[1]])
  a <- as.numeric(parameters[[2]])
  bad <- !is.finite(a) | !is.finite(b) | !(b > a) | a <= 0
  if (any(bad)) stop(sprintf("Invalid parameter rows (need 0 < a < b): %s", paste(which(bad), collapse = ", ")))
  (b - a) / (log(b) - log(a))
}
