#' Read cement end-use shares for Brazil from SNIC (2011)
#'
#' Sindicato Nacional da Industria do Cimento (SNIC): "Relatorio Anual 2011",
#' \url{http://snic.org.br/assets/pdf/relatorio_anual/rel_anual_2011.pdf}.
#' Chart "Aplicacao do cimento - Consumidor final - Brasil 2010" (PDF p. 30), based on the
#' FGV survey "Sondagem do Destino do Cimento". The values are transcribed in \code{values.csv};
#' see \code{SOURCE_INFO.txt}. The category "Others" (2.4 %) is dropped.
#'
#' @author Bennet Weiss
#' @return magpie object of end-use shares in percent, not normalised.
readSNIC2011 <- function() {
  data <- utils::read.csv(file.path("v1", "values.csv"))
  categoryMapping <- c(
    "Residencial / Residential" = "Residential",
    "Comercial, industrial e servicos / Commercial, industrial and services" = "Non-Residential",
    "Infraestrutura / Infrastructure" = "Civil Engineering",
    "Outros / Others" = NA
  )
  return(toolCeEndUseTable(data, categoryMapping))
}
