#' Calculate China plastic production, consumption split and end-of-life shares
#'
#' @description
#' Derive three outputs from the Ren et al. (2025) China material-flow database
#' (\code{\link{readRen2025}}), selected via \code{subtype}:
#' \describe{
#'   \item{\code{"production"}}{Total primary + secondary production (stage
#'     \code{P}, processes \code{domestic} + \code{regeneration}) by type
#'     (\code{Plastics} / \code{Fibre}) and polymer, in tonnes.
#'     The fibre tonnages that stage \code{M} \code{p2m} routes into the
#'     \code{Fibers} products are subtracted from each polymer's plastics production
#'     and reported as separate materials (\code{PET fibre}, \code{Polyamide fibre}
#'     and \code{Other fibre}. Leftover polyamide plastics fold into
#'     \code{Other thermoplastics}.}
#'   \item{\code{"consumption"}}{Share of each polymer and end-use sector within
#'     total apparent consumption (stage \code{U}, process \code{inflow}),
#'     normalized to sum to 1, comparable to \code{\link{calcPlSectorPolymerSplit}}.
#'     Fibres and rubbers are not tracked in the use stage, so only base plastic
#'     polymers appear (type \code{Plastics}).}
#'   \item{\code{"eol"}}{End-of-life treatment shares: the share of domestic
#'     plastic waste (stage \code{W}, process \code{domestic}) that is recycled,
#'     incinerated, landfilled or left untreated, per sector and polymer.}
#' }
#'
#' @param subtype One of \code{"production"}, \code{"consumption"}, \code{"eol"}.
#' @return A list in \code{\link[madrat]{calcOutput}} format. Only China (CHN)
#'   carries data; other countries are 0.
#' @author Leonie Schweiger
#' @seealso \code{\link{readRen2025}}, \code{\link{calcPlSectorPolymerSplit}}
#' @examples
#' \dontrun{
#' a <- calcOutput("PlRen2025", subtype = "production")
#' }
#' @importFrom magclass mselect dimSums add_dimension mbind getItems getSets<-
calcPlRen2025 <- function(subtype) {
  raw <- readSource("Ren2025")

  # helper: fold the transient PA polymer into Other thermoplastics
  .foldPA <- function(x) {
    map <- data.frame(from = getItems(x, dim = "polymer"))
    map$to <- ifelse(map$from == "PA", "Other thermoplastics", map$from)
    toolAggregate(x, rel = map, dim = "polymer", from = "from", to = "to")
  }

  if (subtype == "production") {
    # -------------------------------------------------------------------------
    # Total production (primary + secondary) by base polymer, and the fibre
    # tonnages that manufacturing (p2m) routes into fibre products
    # Note: Although rubbers are also differentiated by Ren et al. (2025),
    # these are only polyurethane rubbers which are part of the "plastics" scope
    # in REMIND-MFA (because covered by PlasticsEurope). Therefore, we keep them in
    # plastics here.
    # -------------------------------------------------------------------------
    collapse <- c("stage", "process", "product", "sector", "disposal")
    prod   <- dimSums(mselect(raw, stage = "P", process = c("domestic", "regeneration")), dim = collapse)
    fibVol <- dimSums(mselect(raw, stage = "M", process = "p2m", product = "Fibers"), dim = collapse)

    # align fibre volumes to the full production polymer set (0 where a polymer has no fibre manufacturing)
    fib <- prod
    fib[, , ] <- 0
    poly <- intersect(getItems(fibVol, dim = "polymer"), getItems(prod, dim = "polymer"))
    fib[, , poly] <- fibVol[, , poly]

    # plastics = production minus the fibre tonnages (floored at 0; in
    # early years p2m fibre manufacturing can exceed a polymer's production)
    plastics <- prod - fib
    plastics[plastics < 0] <- 0
    plastics <- .foldPA(plastics)

    # fibre volumes routed to the named fibre materials (rest -> Other fibre)
    fibMap <- data.frame(from = getItems(fib, dim = "polymer"))
    fibMap$to <- ifelse(fibMap$from == "PET", "PET fibre",
      ifelse(fibMap$from == "PA", "Polyamide fibre", "Other fibre")
    )
    fibre <- toolAggregate(fib, rel = fibMap, dim = "polymer", from = "from", to = "to")

    # tag each block with its type (Plastics / Fibre) and combine
    production <- mbind(
      add_dimension(plastics, dim = 3.1, add = "type", nm = "Plastics"),
      add_dimension(fibre, dim = 3.1, add = "type", nm = "Fibre")
    ) * 1e4 # 10^4 t -> t

    return(list(
      x = production,
      weight = NULL,
      unit = "t",
      description = paste(
        "Total primary + secondary plastic production in China by polymer, from",
        "Ren et al. (2025). Fibre manufacturing tonnages are split off into",
        "separate materials."
      ),
      note = "dimensions: (Time,Region,Type,Material,value)",
      min = 0
    ))
  }

  if (subtype == "consumption" | subtype == "consumption_split") {
    # -------------------------------------------------------------------------
    # Sector-polymer split of apparent consumption (inflow), normalized to sum
    # to 1. Only base plastics (fibres / rubbers untracked in the use stage).
    # -------------------------------------------------------------------------
    cons <- dimSums(mselect(raw, stage = "U", process = "inflow"),
      dim = c("stage", "process", "product", "disposal")
    ) # (region, year, polymer.sector)
    cons <- .foldPA(cons)
    cons <- add_dimension(cons, dim = 3.1, add = "type", nm = "Plastics") * 1e4 # 10^4 t -> t

    if (subtype == "consumption"){
      return(list(
        x = cons,
        weight = NULL,
        unit = "t",
        description = paste(
          "Apparent plastic consumption (inflow) in China by polymer and end-use",
          "sector, from Ren et al. (2025). Fibres and rubbers are not tracked in the",
          "use stage."
        ),
        note = "dimensions: (Time, Region, Type, Material, Good, value)",
        min = 0
      ))
    }

    typeTotal <- dimSums(cons, dim = c("polymer", "sector")) # (region, year, type)
    x <- cons / typeTotal
    x[is.na(x)] <- 0

    # floor zero weights so all-zero regions resolve without NaN (see calcPlSectorPolymerSplit)
    weight <- typeTotal
    weight[weight == 0] <- 1e-9 * max(weight)

    return(list(
      x = x,
      weight = weight,
      unit = "share",
      description = paste(
        "Share of each polymer and end-use sector within total apparent plastic",
        "consumption (inflow) in China, from Ren et al. (2025). Sums to 1 over",
        "(polymer, sector). Fibres and rubbers are not tracked in the use stage."
      ),
      note = "dimensions: (Time, Region, Type, Material, Good, value)",
      min = 0,
      max = 1
    ))
  }

  if (subtype == "eol") {
    # -------------------------------------------------------------------------
    # End-of-life treatment shares: share of domestic waste by disposal route,
    # per sector and polymer.
    # -------------------------------------------------------------------------
    waste <- dimSums(mselect(raw, stage = "W", process = "domestic"),
      dim = c("stage", "process", "product")
    ) # (region, year, polymer.sector.disposal)
    waste <- .foldPA(waste)

    total <- dimSums(waste, dim = "disposal") # (region, year, polymer.sector)
    x <- waste / total
    x[is.na(x)] <- 0

    weight <- total
    weight[weight == 0] <- 1e-9 * max(weight)

    return(list(
      x = x,
      weight = weight,
      unit = "share",
      description = paste(
        "Share of domestic plastic waste treated by each end-of-life route",
        "(Recycled, Incinerated, Landfilled, Untreated) per sector and polymer in",
        "China, from Ren et al. (2025). Sums to 1 over disposal routes."
      ),
      note = "dimensions: (Time, Region, Good, Material, Disposal, value)",
      min = 0,
      max = 1
    ))
  }

  stop("Invalid subtype '", subtype, "' -- supported: production, consumption_split, consumption, eol")
}
