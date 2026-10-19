test_that("a single item is copied to new item names", {
  x <- magclass::new.magpie(
    c("R1", "R2"),
    names = c("a", "b"), sets = c("region", "year", "item"), fill = c(1, 1, 3, 3)
  )

  copied <- toolCopyItem(x = x[, , "a"], newItems = c("c", "d"), newDim = "new")
  expect_equal(magclass::getItems(copied, dim = 3), c("c", "d"))
  expect_equal(magclass::getSets(copied)[[3]], "new")
  expect_equal(as.vector(copied["R2", , ]), c(1, 1))
  expect_error(toolCopyItem(x = x, newItems = c("c", "d"), newDim = "new"), "exactly one")
})
