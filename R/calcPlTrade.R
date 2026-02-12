#' Calculate Country-Level Plastics Trade for Various Categories
#'
#' Reads plastics trade (exports or imports) data at regional level,
#' and backcasts data to 1950 to fill missing years.
#' Note that aggregation to regions is done in the calc functions called by this function,
#' as the source BACI has bilateral trade data that allows to filter out intraregional trade
#' by a custom aggregation function.
#' Therefore, we operate already at the regional aggregation level here (isocountries=FALSE).
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
#' @param source Character; data source:
#'   \itemize{
#'     \item "UNCTAD" - UNCTAD (trade flows by time and region)
#'     \item "BACI" - BACI (trade flows by time, region, sector and polymer)
#'   }
#' @author Qianzhi Zhang
calcPlTrade <- function(
  category,
  flow_label = c("Exports", "Imports"),
  source = c("UNCTAD", "BACI")
) {

  # ---------------------------------------------------------------------------
  # validate inputs
  # ---------------------------------------------------------------------------
  source <- match.arg(source)
  flow_label <- match.arg(flow_label)

  allowed_categories <- list(
    UNCTAD = c("Final", "Primary", "Intermediate", "Manufactured"),
    BACI   = c("Primary", "Application", "Waste")
  )

  if (missing(category)) {
    stop("`category` must be provided.", call. = FALSE)
  }

  if (!category %in% allowed_categories[[source]]) {
    warning(
      sprintf(
        "Invalid category '%s' for source '%s'. Allowed categories are: %s",
        category, source,
        paste(allowed_categories[[source]], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Load data
  # ---------------------------------------------------------------------------
  if (source == "UNCTAD") {
    # Load trade data for the selected category and flow label
    trade <- calcOutput("PlUNCTAD", subtype = category, aggregate=TRUE)
    trade_filtered <- collapseNames(trade[, , getNames(trade, dim = 1) == flow_label])
    # backcast trade data to 1950 based on historic plastic consumption
    consumption <- collapseNames(dimSums(calcOutput("PlConsumptionByGood", aggregate=TRUE), dim = 3))
    x <- toolBackcastByReference(trade_filtered, consumption)

    getNames(x) <- NULL
    note <- "dimensions: (Historic Time,Region,value)"
  } else if (source == "BACI") {
    # Load trade data for the selected category and flow label
    trade <- calcOutput("BACI", subtype = "plastics_UNEP", aggregate=TRUE)
    trade_filtered <- collapseNames(trade[, , list(type = flow_label, stage = category)], preservedim = 4)

    # backcast trade data to 1950 based on historic plastic consumption
    consumption <- calcOutput("PlConsumptionByGood", aggregate = TRUE)

    if (length(getNames(trade_filtered, dim = 2)) == 1 && getNames(trade_filtered, dim = 2) == "General") {
      x <- toolBackcastByReference(trade_filtered,  dimSums(consumption, dim = 3))
    } else {
      x <- toolBackcastByReference(trade_filtered, consumption)
    }

    note <- "dimensions: (Historic Time,Region,Material,Good,value)"

    # remove sector column for Primary category ("General" for all)
    if (category %in% c("Primary", "Waste")) {
      x <- collapseNames(x)
      note <- "dimensions: (Historic Time,Region,Material,value)"
    }
  }

  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
    isocountries = FALSE,
    description = sprintf(
      "%s plastics %s (1950-2023) from %s", category, flow_label, source
    ),
    note = note
  )
}
