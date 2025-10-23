#' Read Data as prepared by Posted
#'
#' https://github.com/PhilippVerpoort/posted
#' Data sources can be found in Posted.
#' @importFrom dplyr %>% all_of select
#' @importFrom tidyr separate unnest
#' @author Bennet Weiss
readPostedBuiltLifespan <- function() {
  path <- file.path("v1", "buildings_and_infrastructure_lifetime.csv")
  data <- suppressMessages(readr::read_csv(path))

  # remove unnecessary colums
  cleaned_data <- data %>%
    select(-all_of(c("period", "unit", "variable")))

  # expand time range into individual years
  cleaned_data <- cleaned_data %>%
    separate(all_of("time_range"), into = c("start_year", "end_year"), sep = "-")
  # create a sequence of years for each row
  cleaned_data$time <- Map(
    seq,
    cleaned_data$start_year,
    cleaned_data$end_year
  )
  # expand this year sequence into separate rows
  expanded_data <- unnest(cleaned_data, cols = c(all_of("time")))

  # remove unnecessary columns
  expanded_data$time_range <- NULL
  expanded_data$start_year <- NULL
  expanded_data$end_year <- NULL

  ordered_data <- expanded_data[, c("region", "time", "end_use", "value")]

  x <- magclass::as.magpie(ordered_data, spatial = 1, temporal = 2)
  return(x)
}
