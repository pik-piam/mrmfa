#' Read Gao & Cabrera-Serrenho 2025 polymer consumption and end-use distribution
#'
#' @description
#' Read data from Gao & Cabrera-Serrenho (2025),
#' doi:10.1016/j.resconrec.2025.108518, data from doi:10.17863/CAM.101645.
#' Two datasets are available via \code{subtype}:
#'
#' \code{"consumption"} (default): apparent polymer consumption ("D") from the
#' MATLAB file \code{D.mat}. D is a 1x8 cell array (one cell per world region);
#' each cell is a 14x44 matrix with rows = 14 polymer groups and columns =
#' years 1978-2021. Apparent consumption = ICIS production + net trade
#' (virgin + recycled), in kilotonnes (kt). Note that, per the paper SI, the
#' years 2020 and 2021 rely on incomplete trade data and are less reliable.
#'
#' \code{"sector_shares"}: the distribution of each polymer across 8 end-use
#' sectors, from the MATLAB file \code{distributionsV3.mat}. The file stores 12
#' named 8x1 vectors (one per polymer, each summing to 1 across the sectors);
#' these are mapped onto the same 14 polymer groups as \code{"consumption"}.
#' The two fibre groups without their own vector (Polyester fibre, Other fibre
#' (acrylic)) reuse the Polyamide-fibre distribution (\code{PPAdist}). The
#' result is not country-resolved, so read it with \code{convert = FALSE}.
#'
#' @author Leonie Schweiger
#' @param subtype Character string selecting the dataset:
#'        \code{"consumption"} (default) returns apparent consumption in kt,
#'        dimensions (region, year, polymer): 8 regions x 44 years x 14 polymers.
#'        \code{"sector_shares"} returns unitless end-use sector shares,
#'        dimensions (sector, polymer): 8 sectors x 14 polymers, each polymer
#'        column summing to ~1.
#' @return MagPIE object (see \code{subtype}).
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' readSource("GaoCabrera2025", subtype = "consumption", convert = FALSE)
#' readSource("GaoCabrera2025", subtype = "sector_shares", convert = FALSE)
#' }
#' @importFrom magclass as.magpie getComment<-
readGaoCabrera2025 <- function(subtype = "consumption") {
  # Polymer labels in D.mat matrix-row order (verified against paper text +
  # SI Table S3). Shared by both subtypes so their polymer dimension matches.
  polymerLabels <- c(
    "LDPE", "LLDPE", "HDPE", "PP", "PS", "PVC", "PET", "PUR",
    "Polyester fibre", "Polyamide fibre", "Other fibre (acrylic)",
    "Rubbers", "Other thermoplastics", "Other thermosets"
  )
  nPoly <- length(polymerLabels) # 14

  x <- switch(subtype,
    "consumption" = {
      # Apparent polymer consumption from D.mat -> magpie (region, year, polymer).
      # Region labels in the cell order of D.mat, matching the `region` column of
      # regionmappingGaoCabrera2025.csv (note "and", not "&").
      regionLabels <- c(
        "North America", "Latin America", "Western and Central Europe",
        "Eastern Europe and Central Asia", "Africa", "Middle East",
        "Northeast Asia", "South Asia and the Pacific"
      )

      years <- 1978:2021 # 44 columns
      nReg <- length(regionLabels) # 8

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

      as.magpie(long, spatial = "region", temporal = "year", datacol = "value")
    },
    "sector_shares" = {
      # End-use sector distribution from distributionsV3.mat -> magpie (sector, polymer).
      # Sector row order of each 8x1 vector.
      sectorLabels <- c(
        "packaging", "transportation", "building and construction",
        "electrical and electronic", "consumer and institutional",
        "industrial machinery", "textile", "other"
      )

      # Map each of the 14 D.mat polymer groups to the source variable in the .mat
      # file. Only 12 vectors exist; the two fibres without a vector (Polyester
      # fibre, Other fibre (acrylic)) reuse the Polyamide-fibre vector (PPAdist).
      polymerToVar <- c(
        "LDPE" = "LDPEdist", "LLDPE" = "LLDPEdist", "HDPE" = "HDPEdist",
        "PP" = "PPdist", "PS" = "PSdist", "PVC" = "PVCdist", "PET" = "PETdist",
        "PUR" = "PURdist", "Polyester fibre" = "PPAdist",
        "Polyamide fibre" = "PPAdist", "Other fibre (acrylic)" = "PPAdist",
        "Rubbers" = "Rubberdist", "Other thermoplastics" = "Otherdist",
        "Other thermosets" = "Epoxydist"
      )

      mat <- R.matlab::readMat("distributionsV3.mat")

      # build a tidy long table (sector, polymer, value = unitless share)
      rows <- lapply(polymerLabels, function(poly) {
        data.frame(
          sector = sectorLabels,
          polymer = poly,
          value = as.vector(mat[[polymerToVar[[poly]]]]),
          stringsAsFactors = FALSE
        )
      })
      long <- do.call(rbind, rows)

      as.magpie(long, datacol = "value")
    },
    stop("Invalid subtype: ", subtype)
  )

  getComment(x) <- subtype
  return(x)
}
