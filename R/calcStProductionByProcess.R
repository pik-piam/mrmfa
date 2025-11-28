#' Calc Steel Production by Process
#' @description
#' Calc steel production by process from WorldSteel datasets. Can be aggregated to regions
#' via calcOutput aggregate parameter. Uses
#' \link{readWorldSteelDigitised} and
#' \link{readWorldSteelDatabase} datasets, the former for
#' historic, the latter for current data. Out of this, the shares of BOF,
#' EAF and Other processes are calculated and interpolated/extrapolated to cover
#' the full time period from 1900-2022. \link{calcStProduction}
#' is used to then multiply the percentages as this data is likely more reliable
#' whilst the data split from the process data is still informative.
#' @param assumedPastPercentages A list of assumed percentages for BOF, EAF,
#' and Other processes for specific missing years. By default, only year 1900 is
#' set to c(0.2, 0, 0.8) (20% BOF, 0% EAF, 80% Other). Currently, data before
#' 1982 is only based on these assumptions
#' @author Merlin Jo Hosak
#' @return Steel production by process across all regions from 1900-2022 as magpie within
#' list of metadata (in calcOutput format).
calcStProductionByProcess <- function(assumedPastPercentages = list("y1900" = c(0.2, 0, 0.8))) {

  # TODO: where will this be used?

  # Internal functions ----

  .cleanMergeProcessData <- function(byProcessData, CurrentData) {

    getNames(byProcessData) <- "value"
    data <- mbind(byProcessData, CurrentData)
    data <- toolInterpolate2D(data, method = "linear")

    return(data)
  }

  .checkPercentagesAddToOne <- function(pct) {
    test <- pct[, , 1] + pct[, , 2] + pct[, , 3] - 1
    if (any(abs(test) > 1e-5)) {
      stop("Percentages do not add to one!")
    }
  }

  .calcPercentagesPerProcess <- function(together, production, assumedPastPercentages) {
    pct <- together / production

    # Overwrite years with no data with global average percentage

    togetherSum <- colSums(together, na.rm = TRUE)
    togetherSumProcesses <- togetherSum[, , 1] + togetherSum[, , 2] + togetherSum[, , 3]
    togetherSumPct <- togetherSum / togetherSumProcesses
    rowTotal <- rowSums(together, na.rm = TRUE) # get countries without any data

    pct[rowTotal == 0, , ] <- togetherSumPct

    # Iterate through assumed past percentages and interpolate to actual values.
    # Assume last value carried forwards for future Extrapolation

    for (idx in 1:3) { # idx refers to processes (BOF, EAF, Other)
      for (year in names(assumedPastPercentages)) {
        pct[, year, idx] <- assumedPastPercentages[[year]][idx]
      }
      pct[, , idx] <- toolInterpolate2D(pct[, , idx], method = "linear")
      # constant percentage towards future (last observation carried forwards/locf)
      pct[, , idx] <- toolInterpolate2D(pct[, , idx], method = "constant")
    }

    # Final check to make sure everything sums up to production values
    .checkPercentagesAddToOne(pct)

    return(pct)
  }

  .createCombinedProcessData <- function(bof, eaf, production) {
    bof[is.na(bof) & !is.na(eaf)] <- 0
    eaf[is.na(eaf) & !is.na(bof)] <- 0

    bofEaf <- bof + eaf
    factor <- production[, getYears(bofEaf), ] / bofEaf

    factor[factor > 1] <- 1

    bof <- bof * factor
    eaf <- eaf * factor

    # new addition is necessary as data might have been scaled down
    other <- production[, getYears(bofEaf), ] - (bof + eaf)

    together <- mbind(bof, eaf, other)

    # clean new dataset
    getNames(together) <- c("BOF", "EAF", "Other")
    missingYears <- setdiff(getYears(production), getYears(together))

    # add missing years and sort them
    together <- add_columns(together, addnm = missingYears, dim = 2) %>%
      magpiesort()

    return(together)
  }

  # Load data ----

  byProcess <- readSource("WorldSteelDigitised", subtype = "productionByProcess")
  bofCurrent <- readSource("WorldSteelDatabase", subtype = "bofProduction")
  eafCurrent <- readSource("WorldSteelDatabase", subtype = "eafProduction")
  production <- calcOutput("StProduction", aggregate = FALSE)

  # Clean data ----

  bof <- .cleanMergeProcessData(byProcess[, , "BOF"], bofCurrent)
  eaf <- .cleanMergeProcessData(byProcess[, , "EAF"], eafCurrent)

  # Combine BOF, EAF, Other
  together <- .createCombinedProcessData(bof, eaf, production)

  # Interpolate missing years via split assumptions
  pct <- .calcPercentagesPerProcess(together, production, assumedPastPercentages)
  productionByProcess <- pct * production

  # Finalize
  final <- list(
    x = productionByProcess,
    weight = NULL,
    unit = "Tonnes",
    description = "Steel production by process from 1900-2022 yearly for the SIMSON format"
  )

  return(final)
}
