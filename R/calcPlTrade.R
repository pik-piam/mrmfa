#' Calculate Country-Level Plastics Trade for Various Categories
#'
#' Reads plastics trade data and backcasts data to 1950 to fill missing years.
#' Note that as the source BACI has bilateral trade data that allows to filter out
#' intraregional trade, aggregation is done by a custom aggregation function for this source.
#'
#' @param category Character; product category:
#'   \itemize{
#'     \item "Final"        - Final plastics
#'     \item "Primary"      - Primary plastics
#'     \item "Intermediate" - Intermediate forms of plastic
#'     \item "Manufactured" - Intermediate manufactured plastic goods
#'     \item "Application"  - Plastic goods
#'     \item "Waste"        - Plastic waste
#'   }
#' @param flow_label Character; trade flow:
#'   \itemize{
#'     \item "Exports" - Exports
#'     \item "Imports" - Imports
#'   }
#' @param data_source Character; data source:
#'   \itemize{
#'     \item "UNCTAD" - UNCTAD (trade flows by time and region)
#'     \item "BACI_UNCTAD" - BACI with UNCTAD HS codes (trade flows by time and region)
#'     \item "BACI_UNEP" - BACI with UNEP HS codes (trade flows by time, region, sector and polymer)
#'   }
#' @param HS Character string specifying the year of the HS (Harmonized System) revision of the data
#'        - 92
#'        - 02
#'        - 17
#'        - 22
#' @param target_years integer vector of target years for the output data.
#' If NULL, all years from reference (plastics production) are included.
#' Note: the 'years' argument in calcOutput does not work properly for this function,
#' because backcasting of trade happens in the custom aggregation function (once the
#' importer + exporter dimension have been aggregated to a single region dimension to
#' match the reference). Since madrat checks whether the years of the returned object
#' cover the years parameter before aggregating, it throws a warning when years that
#' are obtained through backcasting are missing. The target_years parameter is a
#' workaround to fix these warnings - now the warning is thrown if a period is chosen
#' by setting target_years that is not covered by the backcasting data (i.e. production data).
#'
#' @author Qianzhi Zhang, Leonie Schweiger
calcPlTrade <- function(
  category,
  flow_label = c("Exports", "Imports"),
  data_source = c("UNCTAD", "BACI_UNCTAD", "BACI_UNEP"),
  HS = "92",
  target_years = NULL
) {
  # ---------------------------------------------------------------------------
  # validate inputs
  # ---------------------------------------------------------------------------
  data_source <- match.arg(data_source)
  flow_label <- match.arg(flow_label)

  allowed_categories <- list(
    UNCTAD = c("Final", "Primary", "Intermediate", "Manufactured"),
    BACI_UNCTAD = c("Final", "Primary", "Intermediate", "Manufactured", "Waste"),
    BACI_UNEP = c("Primary", "Application", "Waste")
  )

  if (missing(category)) {
    stop("`category` must be provided.", call. = FALSE)
  }

  if (!category %in% allowed_categories[[data_source]]) {
    warning(
      sprintf(
        "Invalid category '%s' for data_source '%s'. Allowed categories are: %s",
        category, data_source,
        paste(allowed_categories[[data_source]], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # define a custom aggregation function that filters out all intra-regional trade
  # and returns both imports and exports for each region in the region mapping
  # in addition, data is backcasted to 1950 based on reference (and eventually
  # forecasted if reference covers more recent years)
  # ---------------------------------------------------------------------------
  # backcast trade data to 1950 based on historic plastic production
  reference <- calcOutput("PlProduction", aggregate = FALSE, years = target_years)

  .customAggregate <- function(x, rel, reference, flow_label) {
    x <- toolAggregateBilateralTrade(x, rel, flow_label)

    ref <- toolAggregate(reference, rel = rel, from = "country", to = "region")

    # if some of the regions are missing in x due to the manual aggregation,
    # fill with NA to match all the ref regions
    missingRegions <- setdiff(getItems(ref, dim = 1), getItems(x, dim = 1))
    if (length(missingRegions) > 0) {
      x <- add_columns(x, addnm = missingRegions, dim = 1, fill = NA)
    }

    x <- toolBackcastByReference(x, ref)
    x <- toolBackcastByReference(x, ref, doForecast = TRUE)

    # cut x to target years
    target_years <- getYears(reference, as.integer = TRUE)
    x <- x[, target_years, ]

    return(x)
  }

  # ---------------------------------------------------------------------------
  # Load data
  # ---------------------------------------------------------------------------
  if (data_source == "UNCTAD") {
    # Load trade data for the selected category and flow label
    trade <- calcOutput("PlUNCTAD", subtype = category, aggregate = FALSE)
    trade_filtered <- collapseNames(trade[, , getNames(trade, dim = 1) == flow_label])
    # backcast trade data to 1950 based on historic plastic production
    reference <- collapseNames(dimSums(reference, dim = 3))
    x <- toolBackcastByReference(trade_filtered, reference)

    getNames(x) <- NULL
    note <- "dimensions: (Historic Time,Region,value)"
    aggregationFunction <- toolAggregate
    aggregationArguments <- NULL
  } else {
    # Load trade data for the selected category
    if (data_source == "BACI_UNEP") {
      x <- calcOutput("PlBACI", subtype = "plastics_UNEP", category = category, HS = HS, aggregate = FALSE)
      # the reference (plastics production) may cover more items than the trade data x
      # (e.g. the reference has type Plastics, Rubber, Fibre while plastic waste trade only has type Plastics).
      # Subset the reference to the items present in x, so backcasting can match dimensions.
      reference <- mselect(reference, type = getItems(x, dim = "type"))
    } else if (data_source == "BACI_UNCTAD") {
      x <- calcOutput("PlBACI", subtype = "plastics_UNCTAD", category = category, HS = HS, aggregate = FALSE)
      reference <- collapseNames(dimSums(reference, dim = 3))
    }

    if (data_source == "BACI_UNEP") {
      note <- "dimensions: (Historic Time,Region,Type,Material,Good,value)"
      # remove sector column for Primary and Waste category ("General" for all)
      if (category %in% c("Primary", "Waste")) {
        x <- collapseNames(x, preservedim = "type")
        note <- "dimensions: (Historic Time,Region,Type,Material,value)"
      }
    } else if (data_source == "BACI_UNCTAD") {
      note <- "dimensions: (Historic Time,Region,value)"
    }

    aggregationFunction <- .customAggregate
    aggregationArguments <- list(reference = reference, flow_label = flow_label)
  }

  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  list(
    x = x,
    weight = NULL,
    unit = "t Plastic",
    aggregationFunction = aggregationFunction,
    aggregationArguments = aggregationArguments,
    description = sprintf(
      "%s plastics %s (1950-2023) from %s", category, flow_label, data_source
    ),
    note = note
  )
}
