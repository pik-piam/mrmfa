#' Read structure type, function type and total floor area of buildings from the Global Exposure Model (GEM).
#' Residential floor area is split into single (RS) and multi family (RM) homes similar to MaterialCities,
#' see \code{\link{toolSingleFamilyShare}}.
#'
#' Release v2026.0.0 (GEM Taxonomy v4.0, building counts and population of 2025).
#' Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, J., Hoyos,
#' M., Martins, L., Paul, N., Rao, A., Silva, V. (2023).
#' Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048
#' Available on Github: https://github.com/gem/global_exposure_model
#' @author Bennet Weiss.
readGlobalExposureModel <- function() {
  version <- "v2026.0.0"
  regions <- list.dirs(path = version, full.names = TRUE, recursive = FALSE)
  regions <- regions[basename(regions) != "World"]
  # GEM uses upper case occupancy classes. NONRES (only New Zealand) has no split into commercial and industrial
  # and is counted as commercial.
  occupancyNames <- c(COM = "Com", IND = "Ind", RES = "Res", NONRES = "Com")
  all_data <- list()

  i <- 1
  for (region in regions) {
    countries <- list.dirs(path = region, full.names = TRUE, recursive = FALSE)
    countries <- countries[basename(countries) != "_region"]
    for (country in countries) {
      # read country data
      path <- file.path(country, "summaries", "Exposure_Summary_Taxonomy.csv")
      if (!file.exists(path)) {
        stop("GEM summary file not found: ", path)
      }
      data <- readr::read_csv(
        path,
        col_names = TRUE,
        col_select = c("ID_0", "OCCUPANCY", "TAXONOMY", "BUILDINGS", "DWELLINGS", "TOTAL_AREA_SQM"),
        show_col_types = FALSE
      )
      stopifnot("Unknown GEM occupancy" = data$OCCUPANCY %in% names(occupancyNames))
      data$OCCUPANCY <- unname(occupancyNames[data$OCCUPANCY])

      # remove rows with unknown area
      data <- data[!is.na(data$TOTAL_AREA_SQM), ]

      # split residential area into single (RS) and multi family (RM) homes, other functions are the occupancy
      isRes <- data$OCCUPANCY == "Res"
      rsShare <- toolSingleFamilyShare(data$TAXONOMY[isRes], data$DWELLINGS[isRes], data$BUILDINGS[isRes])
      singleFamily <- data[isRes, ]
      singleFamily$FUNCTION <- "RS"
      singleFamily$TOTAL_AREA_SQM <- singleFamily$TOTAL_AREA_SQM * rsShare
      multiFamily <- data[isRes, ]
      multiFamily$FUNCTION <- "RM"
      multiFamily$TOTAL_AREA_SQM <- multiFamily$TOTAL_AREA_SQM * (1 - rsShare)
      other <- data[!isRes, ]
      other$FUNCTION <- other$OCCUPANCY
      data <- rbind(singleFamily, multiFamily, other)

      # structure is the material token of the taxonomy, e.g. MUR+ADO or HYB(W;M). It is mapped in convert.
      data$Structure <- sub("/.*$", "", data$TAXONOMY)

      # aggregate floor space
      aggregated_data <- dplyr::group_by(data, .data$ID_0, .data$OCCUPANCY, .data$Structure, .data$FUNCTION) %>%
        dplyr::summarise(TOTALAREA_SQM = sum(.data$TOTAL_AREA_SQM), .groups = "drop")

      all_data[[i]] <- aggregated_data
      i <- i + 1
    }
  }
  combined_data <- do.call(rbind, all_data)
  colnames(combined_data) <- c("ISO3", "Stock_Type", "Structure", "Function", "Total Area (sqm)")

  x <- magclass::as.magpie(combined_data, spatial = 1)

  return(x)
}

#' Share of single family homes (RS) in residential GEM building classes, similar to MaterialCities.
#'
#' Classes with exactly one storey are single family, classes with at least three storeys are multi family.
#' All other classes (they include two storeys, or have no height) are split by dwellings per building:
#' up to 1.5 is single family, from 3 on multi family, linear in between (2.25 gives a share of 0.5).
#' Classes without dwelling or building count get a share of 0.5.
#'
#' @param taxonomy GEM taxonomy strings, height codes as in H:1, H:2-3, H:>5, H:>5+HB:1 or HAPP:25.
#'   Only the storey codes H and HAPP are read, other height codes such as HB are ignored.
#' @param dwellings Number of dwellings of each class.
#' @param buildings Number of buildings of each class.
#' @return Share of single family homes in the floor area of each class, between 0 and 1.
#' @author Bennet Weiss
toolSingleFamilyShare <- function(taxonomy, dwellings, buildings) {
  # height code: "H:1" is one storey, "H:2-3" two to three, "H:>5" more than five (at least 6).
  # Only codes for storeys above ground count, not e.g. HB (below ground) or HF (ground floor height).
  storeyCodes <- paste(c("H", "HAPP"), collapse = "|")
  heightCode <- ifelse(
    grepl(paste0("(^|/)(", storeyCodes, "):"), taxonomy),
    sub(paste0("^(.*/)?(", storeyCodes, "):([^/+]*).*$"), "\\3", taxonomy),
    NA
  )
  oneStorey <- heightCode %in% "1"
  hasNumber <- grepl("^>?[0-9]+", heightCode)
  minStoreys <- rep(NA, length(taxonomy))
  minStoreys[hasNumber] <- as.integer(sub("^>?([0-9]+).*$", "\\1", heightCode[hasNumber])) +
    grepl("^>", heightCode[hasNumber])

  dwellingsPerBuilding <- ifelse(dwellings > 0 & buildings > 0, dwellings / buildings, NA)
  share <- pmin(pmax((3 - dwellingsPerBuilding) / 1.5, 0), 1)
  share[is.na(share)] <- 0.5
  share[oneStorey] <- 1
  share[!is.na(minStoreys) & minStoreys >= 3] <- 0
  return(share)
}
