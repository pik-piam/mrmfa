#' Read UNCTAD
#'
#' Read-in US_PlasticsTradebyPartner file from the
#' United Nations Conference on Trade and Development (UNCTAD).
#' Data is filtered for Trading Partner == World, i.e. all trading partners aggregated.
#'
#' @return magpie object of the UNCTAD data
#'
#' @author Qianzhi Zhang, Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "UNCTAD")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom dplyr if_else
#'
readUNCTAD <- function() {
  # ---------------------------------------------------------------------------
  # Read Data from Excel
  # ---------------------------------------------------------------------------
  data <- read_csv("v2/US_PlasticsTradebyPartner.csv") %>%
    select(1,3,5,7,9,13) %>%
    filter(.data$`Partner Label` == "World" | .data$`Economy Label` == "World") %>%
    dplyr::rename(Region = "Economy Label", Partner_Region = "Partner Label", Flow = "Flow Label", Product = "Product Label")

  # due to splitting of Sudan in 2011, there is "Sudan" with NA entries before 2011 and "Sudan (...2011)" with NA entries after 2011; aggregate them into one region
  data_clean <- data %>%
    mutate(Region = case_when(.data$Region=="Sudan (...2011)" ~ "Sudan", .default = .data$Region),
           Partner_Region = case_when(.data$Partner_Region=="Sudan (...2011)" ~ "Sudan", .default = .data$Partner_Region)) %>%
    group_by(.data$Region, .data$Year, .data$Partner_Region, .data$Flow, .data$Product) %>%
    summarise(Value = sum(.data$`Metric tons in thousands`, na.rm=T)) %>% dplyr::ungroup()

  # data of interest: imports&exports from Region X to World
  # check data based on World to Region X exports and imports
  data_origin <- data_clean %>% filter(.data$Partner_Region == "World") %>% select(-"Partner_Region")
  data_check <- data_clean %>% filter(.data$Region == "World") %>% select(-"Region") %>%
    mutate(Flow = case_when(.data$Flow=="Imports" ~ "Exports", .data$Flow=="Exports"~"Imports"))
  merged <- merge(data_origin, data_check, by.x = c("Region","Year","Flow","Product"), by.y = c("Partner_Region","Year","Flow","Product"), suffix = c("",".check"), all.x=T, all.y=T)%>%
    dplyr::mutate(Year = as.integer(as.character(.data$Year)))
  # some values in the data_check seem to be false, as they are an order of magnitude above the respective data in data_origin and are clear outliers in the whole set
  merged_filtered <- merged %>% filter(.data$Value<1e6) %>%
    dplyr::arrange(.data$Region, .data$Flow, .data$Product, .data$Year) %>%
    group_by(.data$Region, .data$Flow, .data$Product) %>%
    mutate(
      # check whether difference between imports and exports is greater than 5x of median difference
      diff = .data$Value.check-.data$Value,
      diff_median = abs(.data$diff/stats::median(.data$diff)) > 5,
      # check whether difference is greater than 20%
      diff_rel = is.finite(abs(.data$diff/.data$Value)) & abs(.data$diff/.data$Value) > 0.2,
      # if both differences are higher than the threshold, the value is considered implausible; implausible values are interpolated
      implausible = .data$diff_median & .data$diff_rel
    )%>%
    # Replace implausible Value with NA
    mutate(Value_clean = if_else(.data$implausible, NA_real_, .data$Value)) %>%
    # Interpolate over missing (implausible) values
    mutate(Value_interpolated = zoo::na.approx(.data$Value_clean, .data$Year, na.rm = FALSE, rule=2)) %>%
    dplyr::ungroup()

  data_final <- merged_filtered %>% select("Year","Region","Flow","Product","Value_interpolated")


  x <- as.magpie(data_final, temporal = 1, spatial = 2)

  # ---------------------------------------------------------------------------
  # Replace any NA values in the magpie object with 0.
  # ---------------------------------------------------------------------------
  x[is.na(x)] <- 0

  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
