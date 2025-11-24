#' Read World Steel Digitised
#' @description
#' Read Data from World Steel Association 1978-2022 yearbooks digitized to Excel sheets
#' E.g. from 1982: https://worldsteel.org/wp-content/uploads/Steel-Statistical-Yearbook-1982.pdf
#' @param subtype
#' Available subtypes are:
#' 'worldProduction', 'production', 'productionByProcess',
#' 'imports', 'exports', 'scrapImports', 'scrapExports',
#' 'scrapConsumptionYearbooks', 'scrapConsumptionFigures',
#' 'specificScrapConsumption_70s', 'worldScrapConsumption',
#' 'indirectTrade'
#' @author Merlin Jo Hosak, Falk Benke
#'
readWorldSteelDigitised <- function(subtype = "worldProduction") {
  version <- "v1.0"

  # helper functions ----

  # common format: "country_name", followed by year columns
  .readCommonSourceFormat <- function(filenames, type, version) {
    paths <- file.path(version, type, filenames)

    df <- NULL
    for (f in paths) {
      tmp <- readxl::read_excel(path = f) %>%
        tidyr::pivot_longer(c(-"country_name"), names_to = "variable")
      df <- rbind(df, tmp)
    }

    df <- toolCleanSteelRegions(df)

    x <- as.magpie(df, spatial = 1)

    return(x)
  }

  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "worldProduction" = function() {
      path <- file.path(".", version, "production", "world_production_1900-1979.xlsx")
      x <- readxl::read_excel(path = path, range = "A4:B84")
      x <- as.magpie(x)
      # convert from Mt to t
      x <- x * 1e6
      getItems(x, dim = 3) <- "value"
      return(x)
    },
    "production" = function() {
      filenames <- c(
        "production_70s.xlsx",
        "production_80s.xlsx",
        "production_90s.xlsx",
        "production_00s.xlsx"
      )
      x <- .readCommonSourceFormat(filenames, type = "production", version = version)
      x <- x * 1e3 # convert from kt to t

      # fix mislabelled data for 1992-1999 (should be SCG, but is YUG)
      x["SCG", seq(1992, 1999), ] <- x["YUG", seq(1992, 1999), ]
      x["YUG", seq(1992, 1999), ] <- NA

      # fix mislabelled data for 1991-1999 (should be DEU, but is BRG)
      x["DEU", seq(1991, 1999), ] <- x["BRG", seq(1991, 1999), ]
      x["BRG", seq(1991, 1999), ] <- NA
      return(x)
    },
    "productionByProcess" = function() {
      p <- file.path(".", "v1.0", "bof_eaf_production")
      bofLabels <- c("Basic\r\nBessemer\r\nThomas", "Pure\r\nOxygen", "Oxygen")
      eafLabels <- c("Electric")
      otherLabels <- c("Open\r\nHearth\r\nS. M.", "OH", "Other")

      df <- NULL

      # read in data from 1974 - 1982 ----

      for (y in seq(1974, 1981, 1)) {
        f <- readxl::read_excel(path = file.path(p, paste0("Production_by_Process_", y, ".xlsx"))) %>%
          tidyr::pivot_longer(c(-"country_name"), names_to = "variable") %>%
          mutate(
            "year" = y,
            "variable" = replace(.data$variable, .data$variable %in% bofLabels, "BOF"),
            "variable" = replace(.data$variable, .data$variable %in% eafLabels, "EAF"),
            "variable" = replace(.data$variable, .data$variable %in% otherLabels, "Other")
          )
        # sum up variables (e.g. "Basic Bessemer Thomas" and "Pure Oxygen" appear both in some sheet,
        # so count their sum as "BOF")
        # TODO: confirm with JD that this is as intended
        f <- stats::aggregate(value ~ country_name + variable + year, f, sum)

        df <- rbind(df, f)
      }

      # read in data from 80s and 90 ----

      filenames <- c(
        "BOF_production_80s.xlsx",
        "BOF_production_90s.xlsx",
        "EAF_production_80s.xlsx",
        "EAF_production_90s.xlsx"
      )

      for (f in filenames) {
        tmp <- readxl::read_excel(path = file.path(p, f)) %>%
          tidyr::pivot_longer(c(-"country_name"), names_to = "year") %>%
          mutate(
            "variable" = gsub("^([A-Z]{3})_.*", "\\1", f),
            "year" = as.numeric(.data$year)
          )
        df <- rbind(df, tmp)
      }

      df <- toolCleanSteelRegions(df)

      x <- as.magpie(df, spatial = 1)
      x <- x * 1e3 # convert from kt to t
      x <- x[, , c("BOF", "EAF", "Other")]

      return(x)
    },
    "imports" = function() {
      filenames <- c(
        "imports_70s.xlsx",
        "imports_80s.xlsx",
        "imports_90s.xlsx",
        "imports_00s.xlsx"
      )
      x <- .readCommonSourceFormat(filenames, type = "trade", version = version)
      x <- x * 1e3 # convert from kt to t

      return(x)
    },
    "exports" = function() {
      filenames <- c(
        "exports_70s.xlsx",
        "exports_80s.xlsx",
        "exports_90s.xlsx",
        "exports_00s.xlsx"
      )
      x <- .readCommonSourceFormat(filenames, type = "trade", version = version)
      x <- x * 1e3 # convert from kt to t

      return(x)
    },
    "scrapImports" = function() {
      filenames <- c(
        "scrap_imports_70s.xlsx",
        "scrap_imports_80s.xlsx",
        "scrap_imports_90s.xlsx",
        "scrap_imports_00s.xlsx"
      )

      x <- .readCommonSourceFormat(filenames, type = "scrap_trade", version = version)
      x <- x * 1e3 # convert from kt to t

      # fix mislabelled data for 1991-2000 (should be DEU, but is BRG)
      x["DEU", seq(1991, 2000), ] <- x["BRG", seq(1991, 2000), ]
      x["BRG", seq(1991, 2000), ] <- NA

      return(x)
    },
    "scrapExports" = function() {
      filenames <- c(
        "scrap_exports_70s.xlsx",
        "scrap_exports_80s.xlsx",
        "scrap_exports_90s.xlsx",
        "scrap_exports_00s.xlsx"
      )
      x <- .readCommonSourceFormat(filenames, type = "scrap_trade", version = version)
      x <- x * 1e3 # convert from kt to t

      # fix mislabelled data for 1991-2000 (should be DEU, but is BRG)
      x["DEU", seq(1991, 2000), ] <- x["BRG", seq(1991, 2000), ]
      x["BRG", seq(1991, 2000), ] <- NA

      return(x)
    },
    "scrapConsumptionYearbooks" = function() {
      # TODO: make sure, this works with convert as well (by merging subtypes)

      # read in only years that are not superseded by next sheet
      sheetsAndYears <- c(
        "scrap_consumption_75s.xlsx" = list(seq(1975, 1978, 1)),
        "scrap_consumption_80s.xlsx" = list(seq(1979, 1984, 1)),
        "scrap_consumption_85s.xlsx" = list(seq(1985, 1988, 1)),
        "scrap_consumption_90s.xlsx" = list(seq(1989, 1998, 1))
      )
      df <- NULL
      for (f in names(sheetsAndYears)) {
        tmp <- readxl::read_excel(path = file.path(version, "scrap_consumption", f)) %>%
          tidyr::pivot_longer(c(-"country_name"), names_to = "period") %>%
          mutate("period" = as.numeric(.data$period)) %>%
          filter(.data$period %in% sheetsAndYears[[f]])
        df <- rbind(df, tmp)
      }

      df <- df[!duplicated(df), ]
      df <- toolCleanSteelRegions(df)
      x <- as.magpie(df, spatial = 1)
      # manually fix a data error
      # todo: activate, once the comparison is done
      # x["SUN", 1989, ] <- x["SUN", 1989, ] * 1000

      # fix mislabelled data for 1991-1998 (should be DEU, but is BRG)
      x <- add_columns(x, addnm = "DEU", dim = 1, fill = NA)
      x["DEU", seq(1991, 1998), ] <- x["BRG", seq(1991, 1998), ]
      x["BRG", seq(1991, 1998), ] <- NA

      x <- x * 1e3 # convert from kt to t
      return(x)
    },
    "scrapConsumptionFigures" = function() {
      # TODO: make sure, this works with convert as well (by merging subtypes)
      filenames <-
        c(
          "scrap_consumption_2000.xlsx",
          "scrap_consumption_2001.xlsx",
          "scrap_consumption_2002.xlsx",
          "scrap_consumption_2003.xlsx",
          "scrap_consumption_2004.xlsx",
          "scrap_consumption_2005.xlsx",
          "scrap_consumption_2006.xlsx",
          "scrap_consumption_2007.xlsx",
          "scrap_consumption_2008.xlsx"
        )

      df <- NULL
      for (f in filenames) {
        year <- as.numeric(sub("scrap_consumption_([0-9]{4})\\.xlsx", "\\1", f))
        tmp <- readxl::read_excel(path = file.path(
          ".", "v1.0", "scrap_consumption", f
        )) %>%
          tidyr::pivot_longer(c(-"country_name"), names_to = "variable") %>%
          filter(.data$variable == "Consumption") %>%
          mutate("year" = year)

        df <- rbind(df, tmp)
      }

      df <- toolCleanSteelRegions(df)
      x <- as.magpie(df[, c("country_name", "year", "variable", "value")], spatial = 1)
      x <- x * 1e6 # convert from Mt to tonnes

      return(x)
    },
    "specificScrapConsumption70s" = function() {
      # TODO: make sure, this works with convert as well (by merging subtypes)
      filenames <- c("specific_scrap_consumption_70s.xlsx")
      x <- .readCommonSourceFormat(filenames, type = "scrap_consumption", version = version)
      x <- x * 1e-3 # convert from kg/t to t/t (actual share)

      return(x)
    },
    "worldScrapConsumption" = function() {
      x <- readxl::read_excel(
        path = file.path(".", version, "scrap_consumption", "global_scrap_consumption_1975-2008.xlsx"),
        sheet = "Data"
      )
      x <- as.magpie(x)
      x <- x * 1e3 # convert from kT to T
      return(x)
    },
    "indirectTrade" = function() {
      files <- c(
        "WSA_indirect_exports_categories_2013.xlsx",
        "WSA_indirect_imports_categories_2013.xlsx"
      )

      df <- NULL

      for (f in files) {
        path <- file.path(".", version, "indirect_trade_2013", f)

        tmp <- readxl::read_excel(path = path) %>%
          tidyr::pivot_longer(c(-"country_name"), names_to = "variable") %>%
          mutate(
            type = gsub("^WSA_indirect_(.*)_c.*", "\\1", f),
            year = 2013
          )

        df <- rbind(df, tmp)
      }

      df <- toolCleanSteelRegions(df)

      x <- as.magpie(df, spatial = "country_name")

      return(x)
    }
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop("Invalid subtype -- supported subtypes are:", paste0(names(switchboard), collapse = ", "))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
