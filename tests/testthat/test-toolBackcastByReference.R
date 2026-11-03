test_that("backcasting fills only years before the first data year", {
  x <- magclass::new.magpie(c("A", "B"), 2000:2009, fill = NA)
  x["A", 2003:2006, ] <- c(1, 2, 3, 4)
  ref <- magclass::new.magpie(c("A", "B"), 2000:2009, fill = 10)

  backcasted <- toolBackcastByReference(x, ref)
  expect_false(any(is.na(backcasted["A", 2000:2002, ])))
  expect_equal(as.vector(backcasted["A", 2003:2006, ]), c(1, 2, 3, 4))
  expect_true(all(is.na(backcasted["A", 2007:2009, ])))
  # rows without any data are filled completely
  expect_false(any(is.na(backcasted["B", , ])))
})

test_that("forecasting fills only years after the last data year", {
  x <- magclass::new.magpie("A", 2000:2009, fill = NA)
  x["A", 2003:2006, ] <- c(1, 2, 3, 4)
  ref <- magclass::new.magpie("A", 2000:2009, fill = 10)

  forecasted <- toolBackcastByReference(x, ref, doForecast = TRUE)
  expect_true(all(is.na(forecasted["A", 2000:2002, ])))
  expect_false(any(is.na(forecasted["A", 2007:2009, ])))

  # the forecast uses the most recent ratios, not the zeros at the start of the series
  x["A", 2003:2004, ] <- 0
  forecasted <- toolBackcastByReference(toolBackcastByReference(x, ref), ref, doForecast = TRUE)
  expect_true(all(forecasted["A", 2007:2009, ] > 0))
})

test_that("start and end years may differ between regions and data items", {
  x <- magclass::new.magpie(c("A", "B"), 2000:2009, c("p", "q"), fill = NA)
  x["A", 2002:2004, "p"] <- 1
  x["A", 2005:2008, "q"] <- 2
  x["B", 2001:2003, "p"] <- 3
  x["B", 2004:2009, "q"] <- 4
  ref <- magclass::new.magpie(c("A", "B"), 2000:2009, c("p", "q"), fill = 10)

  # backcast: gaps before each start are filled, gaps after each end stay NA
  backcasted <- toolBackcastByReference(x, ref)
  expect_equal(as.vector(backcasted["A", 2000:2001, "p"]), c(1, 1))
  expect_equal(as.vector(backcasted["A", 2000:2004, "q"]), rep(2, 5))
  expect_equal(as.vector(backcasted["B", 2000, "p"]), 3)
  expect_equal(as.vector(backcasted["B", 2000:2003, "q"]), rep(4, 4))
  expect_true(all(is.na(backcasted["A", 2005:2009, "p"])))
  expect_true(all(is.na(backcasted["A", 2009, "q"])))
  expect_true(all(is.na(backcasted["B", 2004:2009, "p"])))

  # forecast afterwards: gaps after each end are filled
  forecasted <- toolBackcastByReference(backcasted, ref, doForecast = TRUE)
  expect_false(any(is.na(forecasted)))
  expect_equal(as.vector(forecasted["A", 2005:2009, "p"]), rep(1, 5))
  expect_equal(as.vector(forecasted["B", 2004:2009, "p"]), rep(3, 6))
})
