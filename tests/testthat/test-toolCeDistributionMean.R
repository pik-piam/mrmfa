# distribution parameters as returned by readCao2024: region x item x parameter
makeParameters <- function(values, parameters, items = "a") {
  x <- magclass::new.magpie(
    cells_and_regions = c("R1", "R2"),
    names = paste(rep(items, each = length(parameters)), parameters, sep = "."),
    sets = c("region", "year", "item", "parameter")
  )
  x[, , ] <- rep(values, each = 2)
  return(x)
}

test_that("uniform mean and harmonic mean match closed forms", {
  x <- makeParameters(c(6, 2), c("max", "min"))
  expect_equal(as.vector(toolCeDistributionMean(x, "Uniform")), c(4, 4))
  expect_equal(as.vector(toolCeDistributionMean(x, "Uniform", harmonic = TRUE)), rep(4 / log(3), 2))
})

test_that("parameter order in the input does not matter", {
  x <- makeParameters(c(2, 6), c("min", "max"))
  expect_equal(as.vector(toolCeDistributionMean(x, "Uniform")), c(4, 4))
})

test_that("triangular mean matches closed form", {
  x <- makeParameters(c(3, 5, 1), c("mode", "max", "min"))
  expect_equal(as.vector(toolCeDistributionMean(x, "Triangular")), c(3, 3))
})

test_that("truncated distributions approach the untruncated mean for wide bounds", {
  weibull <- makeParameters(c(2, 1.5, 0, 1e3), c("scale", "shape", "min", "max"))
  expect_equal(as.vector(toolCeDistributionMean(weibull, "Weibull")), rep(2 * gamma(1 + 1 / 1.5), 2))
  normal <- makeParameters(c(0.4, 0.05, -10, 10), c("mean", "sd", "min", "max"))
  expect_equal(as.vector(toolCeDistributionMean(normal, "Normal")), c(0.4, 0.4))
})

test_that("truncated normal mean is shifted towards the remaining interval", {
  normal <- makeParameters(c(0, 1, 0, 10), c("mean", "sd", "min", "max"))
  expect_equal(as.vector(toolCeDistributionMean(normal, "Normal")), rep(sqrt(2 / pi), 2))
})

test_that("point values are returned unchanged and items are kept", {
  x <- makeParameters(c(1, 2), "value", items = c("a", "b"))
  out <- toolCeDistributionMean(x, "Point")
  expect_equal(magclass::getItems(out, dim = 3), c("a", "b"))
  expect_equal(as.vector(out["R1", , ]), c(1, 2))
})

test_that("mismatching parameters raise an error", {
  x <- makeParameters(c(6, 2), c("max", "min"))
  expect_error(toolCeDistributionMean(x, "Weibull"), "do not match")
  expect_error(toolCeDistributionMean(x, "Triangular", harmonic = TRUE), "not implemented")
})
