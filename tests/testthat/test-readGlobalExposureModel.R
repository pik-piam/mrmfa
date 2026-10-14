test_that("single family share follows storeys and dwellings per building", {
  taxonomy <- c(
    "CR/LFM/H:1/RES", "CR/LFM/H:3/RES", "CR/LFM/H:>4/RES", "CR/LFM/H:>5+HB:1/RES",
    "W/LWAL/H:2/RES", "W/LWAL/H:1-2/RES", "W/LWAL/H:2/RES", "W/LWAL/H:2-3/RES", "W/LWAL/H:2/RES", "MATO/RES"
  )
  dwellings <- c(10, 1, 1, 1, 2.25, 2.7, 1, 5, NA, 2.25)
  share <- toolSingleFamilyShare(taxonomy, dwellings, buildings = rep(1, 10))
  expect_equal(share, c(1, 0, 0, 0, 0.5, 0.2, 1, 0, 0.5, 0.5))
})
