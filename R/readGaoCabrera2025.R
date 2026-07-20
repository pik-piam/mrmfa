#' Read Gao & Cabrera-Serrenho 2025 apparent polymer consumption
#'
#' @description
#' Read the apparent polymer consumption ("D") from Gao & Cabrera-Serrenho (2025),
#' doi:10.1016/j.resconrec.2025.108518, data from doi:10.17863/CAM.101645,
#' from the MATLAB file \code{D.mat}. D is a 1x8 cell array
#' (one cell per world region); each cell is a 14x44 matrix with rows = 14
#' polymer groups and columns = years 1978-2021. Apparent consumption = ICIS
#' production + net trade (virgin + recycled), in kilotonnes (kt).
#' Note that, per the paper SI, the years 2020 and 2021 rely on incomplete trade
#' data and are less reliable.
#'
#' @author Leonie Schweiger
#' @return MagPIE object with apparent consumption in kt, dimensions
#' (region, year, polymer): 8 regions x 44 years x 14 polymers.
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' readSource("GaoCabrera2025", convert = FALSE)
#' }
#' @importFrom magclass as.magpie
readGaoCabrera2025 <- function() {
  # Region labels in the cell order of D.mat, matching the `region` column of
  # regionmappingGaoCabrera2025.csv (note "and", not "&").
  regionLabels <- c(
    "North America", "Latin America", "Western and Central Europe",
    "Eastern Europe and Central Asia", "Africa", "Middle East",
    "Northeast Asia", "South Asia and the Pacific"
  )

  # Polymer labels in matrix-row order (verified against paper text + SI Table S3).
  polymerLabels <- c(
    "LDPE", "LLDPE", "HDPE", "PP", "PS", "PVC", "PET", "PUR",
    "Polyester fibre", "Polyamide fibre", "Other fibre (acrylic)",
    "Rubbers", "Other thermoplastics", "Other thermosets"
  )

  years <- 1978:2021 # 44 columns
  nReg <- length(regionLabels) # 8
  nPoly <- length(polymerLabels) # 14

  mat <- R.matlab::readMat("D.mat")

  # mat$D is a length-8 list; each element is a length-1 list holding a 14x44 matrix.
  cells <- lapply(mat$D, function(cell) {
    mtx <- if (is.list(cell)) cell[[1]] else cell # unwrap the 1-element list
    as.matrix(mtx)
  })

  # build a tidy long table (region, polymer, year, value in kt)
  rows <- lapply(seq_len(nReg), function(m) {
    data.frame(
      region = regionLabels[m],
      polymer = rep(polymerLabels, times = length(years)),
      year = rep(years, each = nPoly),
      value = as.vector(cells[[m]]),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, rows)

  x <- as.magpie(long, spatial = "region", temporal = "year", datacol = "value")

  return(x)
}
