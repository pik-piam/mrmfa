#' Calculate Country-Level Plastics Trade for Various Categories
#'
#' Reads UNCTAD plastics trade (exports or imports) data at regional level,
#' backcasts data to 1950 to fill missing years 1950-2004 years,
#' and aggregates to country level.
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
  if (source=="UNCTAD") {
    # Load trade data for the selected category and flow label
    trade <- calcOutput("PlUNCTAD", subtype = category, aggregate=FALSE)
    trade_filtered <- collapseNames(trade[, , getNames(trade, dim = 1) == flow_label])
    # backcast trade data to 1950 based on historic plastic consumption
    consumption <- collapseNames(dimSums(calcOutput("PlConsumptionByGood", aggregate=FALSE), dim = 3))
    x <- toolBackcastByReference2D(trade_filtered, consumption)

    getNames(x) <- NULL
    note <- "dimensions: (Historic Time,Region,value)"
  }
  else if (source=="BACI") {
    # Load trade data for the selected category and flow label
    trade <- calcOutput("BACI", subtype = "plastics_UNEP", aggregate=FALSE)
    trade_filtered <- collapseNames(trade[, , list(type = tolower(flow_label), stage = category)], preservedim = 4)

    # backcast trade data to 1950 based on historic plastic consumption
    consumption <- calcOutput("PlConsumptionByGood", aggregate=FALSE)
    # loop over dimensions in trade_filtered to use 2D backcast tool
    subset_list <- list()
    k <- 1
    for (s in getNames(trade_filtered, dim = 2)) {
      if (s == "General") {
        consumption_s <- collapseNames(dimSums(consumption, dim = 3))
      } else{
        consumption_s <- collapseNames(consumption[, , getNames(consumption, dim =1) == s])
      }
      for (p in getNames(trade_filtered, dim=1)) {
        if (!(paste(p,s,sep=".") %in% getNames(trade_filtered))) next
        subset <- collapseNames(trade_filtered[, , list(sector = s, polymer = p)])
        subset_b <- toolBackcastByReference2D(subset, consumption_s)
        subset_b <- add_dimension(subset_b, dim = 3.2, add = "sector", nm = s)
        subset_b <- add_dimension(subset_b, dim = 3.1, add = "polymer", nm = p)

        subset_list[[k]] <- subset_b
        k <- k + 1
      }
    }
    x <- do.call(mbind, subset_list)
    note <- "dimensions: (Historic Time,Region,Material,Good,value)"

    # remove sector column for Primary category ("General" for all)
    if (category %in% c("Primary","Waste")) {
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
    description = sprintf(
      "%s plastics %s (1950-2023) from %s", category, flow_label, source
    ),
    note = note
  )
}
