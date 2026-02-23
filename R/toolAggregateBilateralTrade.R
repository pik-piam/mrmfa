#' Aggregate bilateral trade to regional aggregation such that intra-regional trade is filtered out
#'
#' @param x A magpie object with temporal dimension "t", spatial dimension "importer", and
#'          further dimensions "exporter" and custom grouping variables.
#'          The magpie object should actually contain two spatial dimensions "importer" and
#'          "exporter", but since madrat does not support regional aggregation for two spatial
#'          dimensions with iso countries well (as in trade data), we just use one spatial dimension
#'          for the importer
#' @param ref A dataframe with the regional aggregation mapping with columns "country" and "region"
#' @param flow_label The trade type: either "Exports" or "Imports"
#' @return An aggregated magpie object
#' @author Leonie Schweiger
toolAggregateBilateralTrade <- function(x, rel, flow_label) {
  df <- tibble::as_tibble(x)

  # get grouping variables
  group_vars <- setdiff(colnames(df), c("t", "importer", "exporter", "value"))

  # make sure that exporter_region != importer_region for every entry
  df <- df %>%
    left_join(rel[, c("country", "region")], by = c("importer" = "country")) %>%
    left_join(rel[, c("country", "region")], by = c("exporter" = "country")) %>%
    select("t", "importer" = "region.x", "exporter" = "region.y", all_of(group_vars), "value") %>%
    filter(.data$importer != .data$exporter)

  if (flow_label=="Imports"){
    df <- df %>%
      group_by(.data$t, .data$importer, across(all_of(group_vars))) %>%
      summarize(value = sum(.data$value, na.rm = TRUE)) %>%
      ungroup() %>%
      rename("Region" = "importer")
  } else if (flow_label=="Exports"){
    df <- df %>%
      group_by(.data$t, .data$exporter, across(all_of(group_vars))) %>%
      summarize(value = sum(.data$value, na.rm = TRUE)) %>%
      ungroup() %>%
      rename("Region" = "exporter")
  }

  x <- df %>%
    select("Year" = "t", "Region", all_of(group_vars), "value") %>%
    as.magpie()

  return(x)
}
