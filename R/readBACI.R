#' Read BACI Trade data
#'
#' @param subtype Character string specifying the HS (Harmonized System) revision of the data
#'        - HS92
#'        - HS02
#'        - HS17
#'        - HS22
#'
#' @return magpie object of the BACI trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "BACI", subtype = "HS17")
#' }
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter rename summarize
#' @importFrom magclass as.magpie getComment<-
#'
readBACI <- function(subtype = "HS17") {

  # Read raw data
  data_path = paste0("BACI_", subtype, "_V202501")
  files <- list.files(
    data_path,
    pattern = paste0("^BACI_",subtype,"_Y[0-9]{4}_V[0-9]+\\.csv$"),
    full.names = TRUE
  )
  df_all <- rbindlist(
    lapply(files, fread),
    use.names = TRUE,
    fill = TRUE
  )
  # sum up all imports/exports from/to one country per product code; report quantity in Mt
  imports <- df_all %>% group_by(t,j,k) %>%
    summarize(value=sum(q, na.rm=TRUE)/1000000) %>%
    mutate(type = "imports")%>%
    rename(country_code = j)
  exports <- df_all %>% group_by(t,i,k) %>%
    summarize(value=sum(q, na.rm=TRUE)/1000000) %>%
    mutate(type = "exports")%>%
    rename(country_code = i)
  df <- rbind(exports, imports)

  # Read country and product codes of the respective dataset and merge with dataset
  country_codes <- read.csv(file.path(data_path,"country_codes_V202501.csv")) %>% select("country_code","country_iso3")
  product_codes <- read.csv(file.path(data_path, paste0("product_codes_",subtype,"_V202501.csv")))
  df_merge <- df %>%
    merge(country_codes, by="country_code") %>%
    merge(product_codes, by.x="k", by.y="code")
  # country code 490 (country_iso3="S19") is used as a proxy for trade statistics for Taiwan (see https://www.cepii.fr/DATA_DOWNLOAD/baci/doc/baci_webpage.html)
  df_merge <- df_merge %>% mutate(Region = case_when(country_iso3=="S19"~"TWN", .default=country_iso3)) %>%
    select("t", "Region", "type", "k", "description", "value")

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df_merge, temporal = 1, spatial = 2)
  getComment(magpie_data) <- subtype

  return(magpie_data)
}
