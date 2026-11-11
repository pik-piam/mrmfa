test_that("storeys are read from the height code, ranges give their midpoint", {
  taxonomy <- c("CR/H:1/RES", "CR/H:2-3/RES", "CR/H:>5/RES", "CR/H:>5+HB:1/RES", "CR/HAPP:25/RES", "CR/HB:1/RES", "MATO")
  expect_equal(toolGemStoreys(taxonomy), c(1, 2.5, 6, 6, 25, NA, NA))
})

test_that("single family share follows storeys and dwellings per building", {
  taxonomy <- c(
    "CR/LFM/H:1/RES", "CR/LFM/H:4-6/RES", "CR/LFM/H:>3/RES", "CR/LWAL/HAPP:25/RES",
    "W/LWAL/H:2/RES", "W/LWAL/H:2/RES", "W/LWAL/H:3/RES", "W/LWAL/H:1-2/RES", "W/LWAL/H:2-3/RES",
    "W/LWAL/H:2/RES", "MATO/RES", "CR/LWAL/HB:1/RES"
  )
  dwellings <- c(10, 1, 1, 1, 1.5, 2.75, 3, 2.25, 5, NA, 2, 2)
  share <- toolSingleFamilyShare(taxonomy, dwellings, buildings = rep(1, 12))
  # 1 < h < 4: k = 2 h, f = (d - 1.5) / (k - 1.5), share = 3 (1 - f) / (3 (1 - f) + k f)
  # H:2, d = 1.5: f = 0, share = 1
  # H:2, d = 2.75: k = 4, f = 0.5, share = 3/7
  # H:3, d = 3: k = 6, f = 1/3, share = 0.5
  # H:1-2, d = 2.25: k = 3, f = 0.5, share = 0.5
  # H:2-3, d = 5: k = 5, f = 1, share = 0
  expect_equal(share, c(1, 0, 0, 0, 1, 3 / 7, 0.5, 0.5, 0, 0.5, 0.5, 0.5))
})
