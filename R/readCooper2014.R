#' Read Cooper et al. (2014) data
#'
#' @description
#' Read data based on the paper 'Component level strategies for exploiting
#' the lifespan of steel in products'. See notes file in the Cooper 2014
#' folder (notes_on_cooper_lifetime_mean_sd.pdf).
#'
#' @author Merlin Jo Hosak
readCooper2014 <- function() {
  x <- readxl::read_excel(file.path(".", "v1.0", "Cooper_2014_Lifetimes.xlsx"), sheet = "Data") %>%
    tidyr::pivot_longer(cols = c(2, 3)) %>%
    dplyr::rename("variable" = "Cooper Lifetimes", "unit" = "name") %>%
    as.magpie()
  return(x)
}
