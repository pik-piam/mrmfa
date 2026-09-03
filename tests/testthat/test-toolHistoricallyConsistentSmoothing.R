# synthetic data: identical noisy history up to divergeYear, scenario-specific growth after
makeTestData <- function(divergeYear = 2020, scenarios = c("SSP1", "SSP2", "SSP3")) {
  set.seed(42)
  years <- 1950:2050
  x <- magclass::new.magpie(
    cells_and_regions = c("DEU", "FRA"),
    years = years,
    names = scenarios,
    fill = 0,
    sets = c("region", "year", "scenario")
  )
  base <- 100 + (years - 1950)^1.3 + stats::rnorm(length(years), sd = 2)
  growthRates <- c(SSP1 = 0.5, SSP2 = 1, SSP3 = 2)
  for (r in magclass::getItems(x, dim = 1)) {
    for (s in scenarios) {
      v <- base
      late <- years > divergeYear
      v[late] <- v[late] + growthRates[[s]] * (years[late] - divergeYear)^1.5
      x[r, , s] <- v
    }
  }
  x
}

test_that("historical values are identical across scenarios", {
  x <- makeTestData()
  res <- toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020)
  refHist <- as.vector(res[, 1950:2020, "SSP2"])
  for (s in c("SSP1", "SSP3")) {
    expect_equal(as.vector(res[, 1950:2020, s]), refHist)
  }
})

test_that("transition is smooth (no kink) and continuous compared to hard splice", {
  x <- makeTestData()
  res <- toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020, fadeLength = 10)
  hard <- toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020, fadeLength = 1)
  years <- magclass::getYears(res, as.integer = TRUE)

  # kink (second difference) at the transition is smaller for the faded version
  transition <- which(years >= 2015 & years <= 2035)
  kink <- function(obj) max(abs(diff(as.vector(obj["DEU", , "SSP3"]), differences = 2)[transition]))
  expect_gt(kink(hard), kink(res))

  # continuity: value step at the boundary (lastHistYear -> lastHistYear + 1)
  # is much smaller for the faded version than the hard splice
  d1Res  <- diff(as.vector(res["DEU",  , "SSP3"]))
  d1Hard <- diff(as.vector(hard["DEU", , "SSP3"]))
  transIdx <- which(years == 2020) # d1[i] = step from years[i] to years[i+1]
  expect_lt(abs(d1Res[transIdx]), abs(d1Hard[transIdx]))
})

test_that("single scenario and missing scenario dimension pass through", {
  x <- makeTestData()
  x1 <- x[, , "SSP1"]
  expected <- toolTimeSpline(x1, dof = 8)
  expect_equal(
    as.vector(toolHistoricallyConsistentSmoothing(x1)),
    as.vector(expected)
  )
  # object without a "scenario" set: should warn and return plain smoothed result
  x2 <- x1
  magclass::getSets(x2)[3] <- "variable"
  expect_warning(
    expect_equal(
      as.vector(toolHistoricallyConsistentSmoothing(x2)),
      as.vector(expected)
    ),
    "No dimension"
  )
})

test_that("reference scenario is unchanged by the harmonization", {
  x <- makeTestData()
  res <- toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020)
  expected <- toolTimeSpline(x, dof = 8)
  expect_equal(as.vector(res[, , "SSP2"]), as.vector(expected[, , "SSP2"]))
})

test_that("other third-dimension subdimensions are handled correctly", {
  a <- makeTestData()
  magclass::getSets(a)[3] <- "scenario"
  b <- a * 2
  x <- magclass::mbind(
    magclass::addDim(a, dim = 3.2, dimName = "variable", item = "v1"),
    magclass::addDim(b, dim = 3.2, dimName = "variable", item = "v2")
  )
  res <- toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020)
  for (v in c("v1", "v2")) {
    refHist <- as.vector(res[, 1950:2020, paste0("SSP2.", v)])
    for (s in c("SSP1", "SSP3")) {
      expect_equal(as.vector(res[, 1950:2020, paste0(s, ".", v)]), refHist)
    }
  }
  # variables are not mixed up: v2 history stays about twice v1 history
  ratio <- as.vector(res[, 1950:2020, "SSP1.v2"]) / as.vector(res[, 1950:2020, "SSP1.v1"])
  expect_true(all(abs(ratio - 2) < 1e-10))
})

test_that("invalid inputs raise errors and edge cases warn", {
  x <- makeTestData()
  expect_error(
    toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020, refScenario = "SSPX"),
    "not found"
  )
  expect_error(
    toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020, fadeLength = 0),
    "fadeLength"
  )
  expect_warning(
    toolHistoricallyConsistentSmoothing(x, lastHistYear = 2200),
    "replaced by refScenario"
  )
  expect_warning(
    toolHistoricallyConsistentSmoothing(x, lastHistYear = 1900),
    "No years"
  )
})

test_that("lastHistYear is detected automatically when not provided", {
  x <- makeTestData(divergeYear = 2020)
  auto <- toolHistoricallyConsistentSmoothing(x)
  explicit <- toolHistoricallyConsistentSmoothing(x, lastHistYear = 2020)
  expect_equal(as.vector(auto), as.vector(explicit))
})

test_that("verbose reports the detected last historical year", {
  x <- makeTestData(divergeYear = 2020)
  expect_message(
    toolHistoricallyConsistentSmoothing(x, verbose = TRUE),
    "Detected last historical year: 2020"
  )
})

test_that("no common historical period warns and returns plain smoothing", {
  # scenarios already diverge in the first year, so no shared history exists
  x <- makeTestData(divergeYear = 1949)
  expected <- toolTimeSpline(x, dof = 8)
  expect_warning(
    res <- toolHistoricallyConsistentSmoothing(x),
    "no common historical"
  )
  expect_equal(as.vector(res), as.vector(expected))
})

test_that("fade window truncated by end of data works", {
  x <- makeTestData()
  expect_no_error(
    toolHistoricallyConsistentSmoothing(x, lastHistYear = 2045, fadeLength = 20)
  )
})
