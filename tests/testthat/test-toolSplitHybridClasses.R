makeGemData <- function(structure, taxonomy, area) {
  data.frame(ID_0 = "XXX", Structure = structure, TAXONOMY = taxonomy, TOTAL_AREA_SQM = area, stringsAsFactors = FALSE)
}

test_that("non-hybrid classes are unchanged", {
  data <- makeGemData(c("CR+", "MUR", "ADO|ST|E", "OT"), "MAT/LFM/H:1/RES", 10)
  expect_equal(toolSplitHybridClasses(data), data)
})

test_that("hybrid classes are split equally between materials and area is conserved", {
  data <- makeGemData(
    c("HYB", "HYB", "S"),
    c("HYB(W;M)/LWAL/H:1/RES", "HYB(MUR+STRUB;W)/LWAL/H:2/RES", "S/LFM/H:1/RES"),
    c(10, 20, 5)
  )
  out <- toolSplitHybridClasses(data)
  expect_equal(sum(out$TOTAL_AREA_SQM), sum(data$TOTAL_AREA_SQM))
  expect_equal(out$Structure, c("W", "MUR", "MUR", "W", "S"))
  expect_equal(out$TOTAL_AREA_SQM, c(5, 5, 10, 10, 5))
})

test_that("all materials of hybrid classes are mapped to codes of the structure mapping", {
  data <- makeGemData("HYB", "HYB(C;CR;M;MCF;MR;MUR;W;S;EU;ME)/LWAL/H:1/RES", 10)
  out <- toolSplitHybridClasses(data)
  expect_equal(out$Structure, c("CR", "CR", "MUR", "MR|MCF", "MR|MCF", "MUR", "W", "S", "ADO|ST|E", "OT"))
  mapping <- toolGetMapping("CeBuildingStructureMapping.csv", type = "sectoral", where = "mrmfa")
  expect_true(all(out$Structure %in% mapping$GEM_structure))
})

test_that("unknown material in a hybrid class stops", {
  expect_error(toolSplitHybridClasses(makeGemData("HYB", "HYB(W;XYZ)/LFM/H:1/RES", 1)), "Unknown material")
})
