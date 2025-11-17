#' Read data from Cullen et al. (2012)
#' @description
#' Read data from supplementary material of Cullen et al. 2012 paper
#' 'Mapping the global flow of steel: from steelmaking to end‐use goods'.
#' Files were digitalized from pdf to Excel.
#' @param subtype Subtype of Cullen et al. (2012) data to load. Currently
#' supported subtypes are "flows", "giMatrix"
#' @author Merlin Jo Hosak
readCullen2012 <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  version <- "v1.0"
  switchboard <- list(
    "flows" = function() {
      path <- file.path(".", version, "Cullen_2012_Flows.xlsx")
      df <- readxl::read_excel(path = path, sheet = "Data") %>%
        mutate(
          "Description" = ifelse(is.na(.data$Description), "", paste0(" (", .data$Description, ")")),
          "identifier" = paste0(.data$ID, ": ", .data$Source, " -> ", .data$Target, .data$Description),
          # remove dots, as they are reserved separators in magpie objects
          "identifier" = gsub("\\.", "", identifier)
        ) %>%
        select("identifier", "value" = "Value")

      x <- as.magpie(df)
      return(x)
    },
    "giMatrix" = function() {
      path <- file.path(".", version, "Cullen_2012_GI_Matrix.xlsx")
      df <- readxl::read_excel(
        path = path,
        sheet = "Data"
      )
      df_long <- tidyr::pivot_longer(
        df,
        cols = -1, # all columns except the first
        names_to = "variable", # column headers become values here
        values_to = "value" # cell values here
      )

      giMatrix <- as.magpie(df_long)

      return(giMatrix)
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
