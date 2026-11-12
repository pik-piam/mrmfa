#' Calculates how cement consumption is split into end uses
#'
#' Splits cement consumption into the end uses Residential, Commercial, Industrial and
#' Civil Engineering for each country and historical year.
#'
#' \enumerate{
#'   \item Country data, in order of priority: CEMBUREAU 2023 (EU27 members except Germany),
#'     VDZ 2021 (Germany), CRISIL FY21 and FY25 (India), SNIC 2010 (Brazil), Xi et al. (2016)
#'     (China, average, constant over time) and Cao et al. (2017) for all digitised country series (incl. USA).
#'   \item For countries with data, the years in between are interpolated linearly. Outside the data
#'     range, the first (last) value is held constant.
#'   \item Countries without data get a split that is linear in GDP per capita between India (mean of FY21 and FY25)
#'     and the EU27 (2023). It is evaluated at the mean GDP per capita of 2006-2025 and held
#'     constant over time. Above the EU27 GDP per capita, the EU27 split is kept.
#'   \item Non-Residential is split into Commercial and Industrial with the industrial share of
#'     Xi et al. (2016) for China (average), for all countries.
#' }
#'
#' @author Bennet Weiss
#' @return List with magpie object of the split (region x year x end use) and metadata.
calcCeEndUseSplit <- function() {
  mainEndUses <- c("Residential", "Non-Residential", "Civil Engineering")
  eu27 <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
    "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )
  # India reference: mean of FY21 (2020, Covid year, high rural housing) and FY25 (2024, high
  # infrastructure share due to government capital expenditure), to even out both distortions
  indiaReferenceYears <- c(2020, 2024)
  euReferenceYear <- 2023
  # countries without data get a split that is constant over time, based on their mean GDP per capita
  # in these years (within-country changes in the data do not follow GDP, but housing cycles)
  gdppcMeanYears <- 2006:2025

  weight <- calcOutput("CeCementProduction", aggregate = FALSE)
  years <- getYears(weight, as.integer = TRUE)
  countries <- getItems(weight, dim = 1)

  cembureau <- readSource("CEMBUREAU2025")
  vdz <- readSource("VDZ2022")
  crisil <- readSource("CRISIL2025")
  snic <- readSource("SNIC2011")
  xi <- readSource("Xi2016")
  cao <- readSource("Cao2017")

  # 1. collect country data; sources added later only fill country-years that are still empty
  data <- new.magpie(countries, years, mainEndUses, fill = NA, sets = c("region", "year", "End Use"))
  fillIfMissing <- function(data, x, regions) {
    dataYears <- intersect(getYears(x), getYears(data))
    target <- data[regions, dataYears, ]
    missing <- is.na(target)
    target[missing] <- x[regions, dataYears, mainEndUses][missing]
    data[regions, dataYears, ] <- target
    return(data)
  }
  data <- fillIfMissing(data, cembureau, setdiff(eu27, "DEU"))
  data <- fillIfMissing(data, vdz, "DEU")
  data <- fillIfMissing(data, crisil, "IND")
  data <- fillIfMissing(data, snic, "BRA")
  # Xi et al. (2016): average split for China (no year), used for all years
  xiChina <- xi["CHN", , ]
  chinaSplit <- data["CHN", , ]
  chinaSplit[, , "Residential"] <- magpie_expand(xiChina[, , "Residential"], chinaSplit[, , "Residential"])
  chinaSplit[, , "Civil Engineering"] <- magpie_expand(xiChina[, , "Civil Engineering"], chinaSplit[, , "Civil Engineering"])
  chinaSplit[, , "Non-Residential"] <- magpie_expand(
    dimSums(xiChina[, , c("Commercial", "Industrial")], dim = 3), chinaSplit[, , "Non-Residential"]
  )
  data["CHN", , ] <- chinaSplit
  # Cao et al. (2017): all digitised country series are used (incl. USA). Some have unusually low
  # residential shares (BGR, CZE, HUN, SWE: <= 11 %), which likely reflects national building
  # classifications; no other source contradicts them.
  data <- fillIfMissing(data, cao, getItems(cao, dim = 1))

  # 2. interpolate between data years; outside the data range, hold the first (last) value constant
  # (countries without data stay NA)
  data <- toolInterpolate(data, type = "linear", extrapolate = TRUE)

  # 3. countries without data: linear in GDP per capita between India and EU27
  gdppc <- calcOutput("CoGDP", perCapita = TRUE, scenarios = "SSP2", smooth = TRUE, aggregate = FALSE)
  population <- calcOutput("CoPopulation", scenarios = "SSP2", smooth = TRUE, aggregate = FALSE)
  gdppc <- collapseDim(gdppc[countries, years, ], dim = 3)
  euPopulation <- collapseDim(population[eu27, euReferenceYear, ], dim = 3)
  gdppcEu <- sum(gdppc[eu27, euReferenceYear, ] * euPopulation) / sum(euPopulation)
  gdppcIndia <- mean(as.numeric(gdppc["IND", indiaReferenceYears, ]))
  # CEMBUREAU reports the EU27 aggregate, which the convert function assigns to every member state
  splitEu <- collapseDim(cembureau["FRA", euReferenceYear, mainEndUses], dim = 1:2)
  splitIndia <- collapseDim(magpply(crisil["IND", indiaReferenceYears, mainEndUses], mean, DIM = 2), dim = 1:2)
  gdppcMean <- magpply(gdppc[, gdppcMeanYears, ], mean, DIM = 2)
  gdpWeight <- (gdppcMean - gdppcIndia) / (gdppcEu - gdppcIndia)
  gdpWeight[gdpWeight > 1] <- 1
  gdpLine <- magpie_expand(splitIndia + gdpWeight * (splitEu - splitIndia), data)
  noData <- is.na(data)
  data[noData] <- gdpLine[noData]
  if (anyNA(data)) stop("End-use split contains NA after filling.")
  if (!isTRUE(all.equal(data, toolCeNormalizeEndUseSplit(data)))) stop("End-use split is not normalized.")

  # 4. split non-residential buildings into commercial and industrial (Xi et al. 2016, China)
  industrialRatio <- as.numeric(
    xi["CHN", , "Industrial"] / (xi["CHN", , "Commercial"] + xi["CHN", , "Industrial"])
  )
  nonResidential <- data[, , "Non-Residential"]
  x <- mbind(
    data[, , "Residential"],
    setNames(nonResidential * (1 - industrialRatio), "Commercial"),
    setNames(nonResidential * industrialRatio, "Industrial"),
    data[, , "Civil Engineering"]
  )

  weight <- magpie_expand(weight, x)
  weight[weight == 0] <- 1e-10

  unit <- "ratio"
  description <- paste(
    "Split of cement consumption into the end uses Residential, Commercial, Industrial and Civil Engineering.",
    "Country data from CEMBUREAU (2025), VDZ (2022), CRISIL (2025), SNIC (2011), Xi et al. (2016) and",
    "Cao et al. (2017), interpolated between data years; outside the data range, the last value kept constant.",
    "Countries without data: linear in mean GDP per capita 2006-2025 between",
    "India (CRISIL, mean of FY21 and FY25) and EU27 (CEMBUREAU 2023), constant over time.",
    "Commercial/industrial split of non-residential buildings from Xi et al. (2016) for China."
  )
  note <- "dimensions: (Historical Time,Region,End Use,value)"

  return(list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 0,
    max = 1
  ))
}
