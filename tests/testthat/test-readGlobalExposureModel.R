test_that("residential type follows the number of stories", {
  data <- data.frame(
    OCCUPANCY = c("Res", "Res", "Res", "Res", "Com", "Res"),
    TAXONOMY = c("CR/H:1/RES", "CR/H:2/RES", "CR/H:3/RES", "CR/H:>8/RES:3", "CR/H:1/COM", "CR/H:1-2/RES"),
    stringsAsFactors = FALSE
  )
  expect_equal(toolInferResBuildingType(data), c("RS", "RS", "RM", "RM", "Com", "RS"))
})
