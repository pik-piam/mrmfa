#' Read World Steel Digitised
#' @description
#' Read Data from World Steel Association 1978-2022 yearbooks digitized to Excel sheets
#' E.g. from 1982: https://worldsteel.org/wp-content/uploads/Steel-Statistical-Yearbook-1982.pdf
#' @param subtype Character string defining which subtype of data to read.
#' Available subtypes are:
#' 'worldProduction', 'production', 'bofProduction', 'eafProduction',
#' 'productionByProcess', 'imports', 'exports', 'scrapImports', 'scrapExports',
#' 'scrapConsumptionYearbooks', 'scrapConsumptionFigures',
#' 'specificScrapConsumption_70s', 'worldScrapConsumption',
#' 'indirectImportsByCategory_2013', 'indirectExportsByCategory_2013'
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @importFrom rlang .data
#' @importFrom dplyr cur_data
readWorldSteelDigitised <- function(subtype = "worldProduction") {
  version <- "v1.0"
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "worldProduction" = function() {
      path <- paste0(
        "./", version, "/production/",
        "world_production_1900-1979.xlsx"
      )
      x <- readxl::read_excel(
        path = path,
        range = "A4:B84"
      )
      x <- as.magpie(x)

      # convert from Mt to t
      x <- x * 1e6

      getItems(x, dim = 3) <- "value"

      return(x)
    },
    "production" = function() {
      filenames <- paste0(
        c(
          "production_70s",
          "production_80s",
          "production_90s",
          "production_00s"
        ),
        ".xlsx"
      )
      production <- toolLoadWorldSteelDigitised(filenames, type = "production", version = version)
      return(production)
    },
    "bofProduction" = function() {
      filenames <- paste0(
        c(
          "BOF_production_80s",
          "BOF_production_90s"
        ),
        ".xlsx"
      )
      bofProduction <- toolLoadWorldSteelDigitised(filenames, type = "bof_eaf_production", version = version)
      return(bofProduction)
    },
    "eafProduction" = function() {
      filenames <- paste0(
        c(
          "EAF_production_80s",
          "EAF_production_90s"
        ),
        ".xlsx"
      )
      eafProduction <- toolLoadWorldSteelDigitised(filenames, type = "bof_eaf_production", version = version)
      return(eafProduction)
    },
    "productionByProcess" = function() {

      bofLabels <- c("Basic\r\nBessemer\r\nThomas", "Pure\r\nOxygen", "Oxygen")
      eafLabels <- c("Electric")
      otherLabels <- c("Open\r\nHearth\r\nS. M.", "OH", "Other")

      x <- new.magpie(cells_and_regions = c(
        madrat::getISOlist(), c("BRG", "GDR", "SUN", "YUG")),
        years = seq(1974, 1981, 1),
        names = c("BOF", "EAF", "Other"))

      for (y in seq(1974, 1981, 1)) {
        f <- readxl::read_excel(path = file.path(".", "v1.0", "bof_eaf_production",
                                                 paste0("Production_by_Process_", y, ".xlsx"))) %>%
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

        f <- as.magpie(f[, c("country_name", "year", "variable", "value")], spatial = 1) %>%
          toolFoo()

        x[getItems(f, dim = 1), y, ] <- f[, , c("BOF", "EAF", "Other")]

      }

      x <- x * 1e3 # convert from kt to t

      # remove all-NA regions
      remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
      x <- x[!remove, , ]

      return(x)
    },
    "imports" = function() {
      filenames <- paste0(
        c(
          "imports_70s",
          "imports_80s",
          "imports_90s",
          "imports_00s"
        ),
        ".xlsx"
      )
      imports <- toolLoadWorldSteelDigitised(filenames, type = "trade", version = version)
      return(imports)
    },
    "exports" = function() {
      filenames <- paste0(
        c(
          "exports_70s",
          "exports_80s",
          "exports_90s",
          "exports_00s"
        ),
        ".xlsx"
      )
      exports <- toolLoadWorldSteelDigitised(filenames, type = "trade", version = version)
      return(exports)
    },
    "scrapImports" = function() {
      filenames <- paste0(
        c(
          "scrap_imports_70s",
          "scrap_imports_80s",
          "scrap_imports_90s",
          "scrap_imports_00s"
        ),
        ".xlsx"
      )
      scrapImports <- toolLoadWorldSteelDigitised(filenames, type = "scrap_trade", version = version)
      return(scrapImports)
    },
    "scrapExports" = function() {
      filenames <- paste0(
        c(
          "scrap_exports_70s",
          "scrap_exports_80s",
          "scrap_exports_90s",
          "scrap_exports_00s"
        ),
        ".xlsx"
      )
      scrapExports <- toolLoadWorldSteelDigitised(filenames, type = "scrap_trade", version = version)
      return(scrapExports)
    },
    "scrapConsumptionYearbooks" = function() {
      filenames <- paste0(
        c(
          "scrap_consumption_75s",
          "scrap_consumption_80s",
          "scrap_consumption_85s",
          "scrap_consumption_90s"
        ),
        ".xlsx"
      )

      # Combine 5 year steps into one via loader function
      # Even though datasets are ten years each, merging works and the more recent data is taken (overwrites the old one)
      scrapConsumption <- toolLoadWorldSteelDigitised(filenames, type = "scrap_consumption", version = version)
      return(scrapConsumption)
    },
    "scrapConsumptionFigures" = function() {
      filenames <- paste0(
        c(
          "scrap_consumption_2000",
          "scrap_consumption_2001",
          "scrap_consumption_2002",
          "scrap_consumption_2003",
          "scrap_consumption_2004",
          "scrap_consumption_2005",
          "scrap_consumption_2006",
          "scrap_consumption_2007",
          "scrap_consumption_2008"
        ),
        ".xlsx"
      )
      years <- paste0("y", 2000:2008)

      year_data <- comprehenr::to_list(
        for (i in seq_along(filenames)) {
          toolLoadWorldSteelFiguresData(filenames[i], years[i], type = "Consumption")
        }
      )

      x <- toolWSDecadeMerge(year_data)

      # remove IAF and IAS (other Asia and Africa, due to inconsistent madrat mapping)
      x <- x[!rownames(x) %in% c("IAF", "IAS"), ]

      x <- x * 1e6 # convert from Mt to tonnes

      return(x)
    },
    "specificScrapConsumption70s" = function() {
      path <- paste0("./", version, "/scrap_consumption/", "specific_scrap_consumption_70s.xlsx")
      x <- readxl::read_excel(path = path)
      x <- as.magpie(x, spatial = "country_name")

      # ignore super-regions
      countries <- getItems(x, dim = 1)

      ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"))$reg
      browser()
      getItems(x, dim = 1) <- toolCountry2isocode(countries, ignoreCountries = ignore)

      # Remove NA rows
      x <- x[!is.na(getItems(x, dim = 1)), ]

      x <- x * 1e-3 # convert from kg/t to t/t (actual share)

      return(x)
    },
    "worldScrapConsumption" = function() {
      x <- readxl::read_excel(
        path = paste0(
          "./v1.0/scrap_consumption/",
          "global_scrap_consumption_1975-2008.xlsx"
        ),
        sheet = "Data"
      )
      x <- as.magpie(x)
      x <- x * 1e3 # convert from kT to T
      return(x)
    },
    "indirectImportsByCategory2013" = function() {
      x <- toolLoadIndirectTrade2013("indirect_imports", version = version)
      return(x)
    },
    "indirectExportsByCategory2013" = function() {
      x <- toolLoadIndirectTrade2013("indirect_exports", version = version)
      return(x)
    },
    NULL
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

# ---- Functions ----

toolLoadWorldSteelDigitised <- function(filenames, type, version) {
  paths <- paste0(version, "/", type, "/", filenames)
  decades <- comprehenr::to_list(for (path in paths) toolWSDecadeRead(path))
  x <- toolWSDecadeMerge(decades)
  x <- x * 1e3 # convert from kt to t
  return(x)
}

toolWSDecadeRead <- function(name) {

  x <- readxl::read_excel(path = name)
  # delete unnecessary rows (total or other in the name or NA)
  x <- x %>%
    filter(!grepl("total|other", cur_data()[[1]], ignore.case = TRUE),
           !is.na(.data$country_name))

  # convert to magpie
  x <- as.magpie(x, spatial = 1)

  x <- toolFoo(x)

  return(x)
}

toolWSDecadeMerge <- function(magpies) {
  # get merged countries & years
  countries <- character(0)
  years <- character(0)
  for (magpie in magpies) {
    countries <- union(countries, getItems(magpie, dim = 1))
    years <- union(years, getItems(magpie, dim = 2))
  }

  # sort the indices
  countries <- sort(countries)
  years <- sort(years)

  # create a new magpie object with appropriate size
  x <- new.magpie(
    cells_and_regions = countries,
    years = years,
    names = "value",
    fill = NA,
    sets = names(dimnames(magpies[[1]]))
  )

  # fill in the data
  for (magpie in magpies) {
    x[getItems(magpie, dim = 1), getItems(magpie, dim = 2)] <- magpie
  }

  return(x)
}

toolLoadWorldSteelFiguresData <- function(filename, year, type) {
  x <- readxl::read_excel(path = paste0(
    "./v1.0/scrap_consumption/",
    filename
  ))
  x <- as.magpie(x, spatial = "country_name")
  x <- x[, , type]
  countries <- getItems(x, dim = 1)

  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"))$reg

  browser()
  getItems(x, dim = 1) <- toolCountry2isocode(countries, ignoreCountries = ignore)
  getItems(x, dim = 2) <- year
  getItems(x, dim = 3) <- "value"

  x <- x[!is.na(getItems(x, dim = 1)), ]

  return(x)
}

toolLoadIndirectTrade2013 <- function(subtype, version) {
  path <- paste0(
    "./",
    version,
    "/indirect_trade_2013/",
    "WSA_",
    subtype,
    "_categories_2013.xlsx"
  )
  x <- readxl::read_excel(path = path)

  # delete unnecessary rows (total or other in the name or NA)
  keep_rows <- !grepl("total|other", x[[1]], ignore.case = TRUE)
  keep_rows[is.na(keep_rows)] <- FALSE
  x <- x[keep_rows, , drop = FALSE]
  x <- x[!is.na(x$country_name), , drop = FALSE]

  x <- as.magpie(x, spatial = "country_name")

  x <- add_columns(x, addnm = c("Construction", "Machinery", "Transport", "Products", "Total"), dim = "variable")

  x[, , "Construction"] <- 0
  x[, , "Machinery"] <- x[, , "Mechanical Machinery"]
  x[, , "Transport"] <- x[, , "Automotive"] + x[, , "Other transport"]
  x[, , "Products"] <- x[, , "Electrical Equipment"] + x[, , "Metal products"] + x[, , "Domestic appliances"]
  x[, , "Total"] <- x[, , "Machinery"] + x[, , "Transport"] + x[, , "Products"]

  # calc shares
  x[, , "Machinery"] <- x[, , "Machinery"] / x[, , "Total"]
  x[, , "Transport"] <- x[, , "Transport"] / x[, , "Total"]
  x[, , "Products"] <- x[, , "Products"] / x[, , "Total"]

  # drop unnecessary columns
  x <- x[, , c("Construction", "Machinery", "Products", "Transport")]

  countries <- getItems(x, dim = 1)
  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"))$reg
  browser()
  getItems(x, dim = 1) <- toolCountry2isocode(countries, ignoreCountries = ignore)

  # remove rows with NA in country_name column
  x <- x[!is.na(getItems(x, dim = 1)), ]

  return(x)
}

