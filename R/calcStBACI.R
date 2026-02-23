#' Split World Steel trade into bilateral trade by using proxy bilateral BACI trade data
#' and aggregate via a custom aggregation function to filter out intra-regional trade
#' (since we don't have a list with all HS codes and corresponding steel shares,
#' BACI trade data cannot be used directly, since it does not cover the full scope)
#'
#' @param subtype Character string specifying the scope
#'        - imports
#'        - exports
#' @param category Character string specifying the stage of trade
#'        - direct
#'        - indirect
#'        - scrap
#' @param HS Character string specifying the year of the HS (Harmonized System) revision of the data
#'        - 92
#'        - 02
#'        - 17
#'        - 22
#'
#' @return magpie object of the aggregated trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [calcOutput()]
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "StBACI", subtype = "imports",
#' category = "direct", HS = "02")
#' }
#' @importFrom dplyr select filter rename summarize ungroup
#' @importFrom magclass as.magpie getComment<-
#'
calcStBACI <- function(subtype, category, HS="92") {

  # Read World Steel trade data
  # map category
  subtype_WS <- switch(category,
                       "indirect" = paste0(category,stringr::str_to_title(subtype)),
                       "scrap" = paste0(category,stringr::str_to_title(subtype)),
                       "direct" = subtype,
                       stop("Unsupported category: ", category)
  )
  WS_trade <- calcOutput("StTrade", subtype = subtype_WS, aggregate=FALSE)
  WS_trade_df <- WS_trade %>% as.data.frame() %>%
    rename("t" = "Year") %>% select(-"Cell") %>% mutate(t = as.integer(as.character(.data$t)))
  if(category=="indirect"){
    WS_trade_df <- WS_trade_df %>% rename("sector"="Data1")
  }

  # Read raw BACI data
  weights <- readSource("BACI", subtype = paste("steel", category, sep="-"), subset = HS) %>%
    quitte::madrat_mule() %>%
    rename("Region" = case_when(subtype=="imports"~"importer", subtype=="exports"~"exporter"),
           "Region2" = case_when(subtype=="imports"~"exporter", subtype=="exports"~"importer"))
  # get grouping variables
  group_vars <- setdiff(colnames(weights), c("t", "Region","Region2","value"))

  # calculate weights to split WS trade data into bilateral trade data
  weights <- weights %>% group_by(.data$t, .data$Region, across(all_of(group_vars))) %>%
    mutate(total = sum(.data$value), weight = .data$value/.data$total) %>% ungroup() %>%
    select(-"total",-"value")
  # historical ISO countries SCG and ANT split into SRB & MNE in 2006 and SXM & CUW in 2011, respectively
  # for simplicity, Region2 is assigned to their major successor countries SRB and CUW before the split year
  weights$Region2[weights$Region2 == "SCG"] <- "SRB"
  weights$Region2[weights$Region2 == "ANT"] <- "CUW"
  # the shares for Region SCG and ANT are mapped to both SRB & MNE and SXM & CUW
  weights <- rbind(weights %>% filter(.data$Region != "SCG", .data$Region != "ANT"),
                   weights %>% filter(.data$Region == "SCG") %>% mutate(Region="SRB"),
                   weights %>% filter(.data$Region == "SCG") %>% mutate(Region="MNE"),
                   weights %>% filter(.data$Region == "SCG") %>% mutate(Region="SXM"),
                   weights %>% filter(.data$Region == "SCG") %>% mutate(Region="CUW"))

  # split WS trade data into bilateral trade
  df <- left_join(WS_trade_df %>% filter(t >= min(weights$t)), weights, by=c("t","Region",all_of(group_vars))) %>%
    mutate(value = .data$weight * .data$Value)
  # check whether there are missing weights (e.g. LUX and ZAF are missing in the first years)
  missing <- df %>% filter(is.na(value) & Value>0)

  df <- df %>%
    select(-"weight",-"Value") %>%
    rename("importer" = case_when(subtype=="imports"~"Region", subtype=="exports"~"Region2"),
           "exporter" = case_when(subtype=="imports"~"Region2", subtype=="exports"~"Region"))

  if(category=="indirect"){
    note <- "dimensions: (Historic Time,Region,Good,value)"
  } else {
    df <- df %>% select(-"Data1")
    note <- "dimensions: (Historic Time,Region,value)"
  }

  x <- as.magpie(df, temporal = "t", spatial = "importer")
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  x <- replace_non_finite(x, replace = 0)

  .customAggregate <- function(x, rel, reference, flow_label) {

    # aggregate to regions filtering out intra-regional trade
    x <- toolAggregateBilateralTrade(x, rel, flow_label)

    # backcast aggregated bilateral trade data to 1900 based on WS trade data
    ref <- toolAggregate(reference, rel = rel)

    # if some of the regions are missing in x due to the manual aggregation,
    # fill with NA to match all the ref regions
    missingRegions <- setdiff(getItems(ref, dim = 1), getItems(x, dim = 1))
    if (length(missingRegions) > 0) {
      x <- add_columns(x, addnm = missingRegions, dim = 1, fill = NA)
    }

    x <- toolBackcastByReference(x, ref)

    return(x)
  }

  return(list(
    x = x,
    weight = NULL,
    aggregationFunction = .customAggregate,
    aggregationArguments = list(reference = WS_trade, flow_label = stringr::str_to_title(subtype)),
    unit = "Tonnes",
    description = paste("Steel trade:", category, subtype, "from 1900-2021 yearly.", sep=" "),
    note = note
  ))
}
