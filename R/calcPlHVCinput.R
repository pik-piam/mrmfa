#' Calculates the parameters that determine the HVC input to virgin plastics production
#' based on data by Levi and Cullen (2018) https://doi.org/10.1021/acs.est.7b04573
#' @author Leonie Schweiger
#' @param subtype Character string specifying the parameter to read
#'        - "polymerization_yield"
#'        - "HVC_input_ratio"
#'        - "C4_input_ratio"
calcPlHVCinput <- function(subtype) {

  df <- readSource("LeviCullen", subtype = "HVCbyProcess", convert = FALSE) %>% as.data.frame() %>%
    rename("output" = "Data1", "input" = "Data2", "type" = "Data3", "coef" = "Value") %>%
    mutate(output = as.character(output), input = as.character(input), type = as.character(type)) %>%
    select(-"Cell",-"Region",-"Year",-"Data4")

  # ---------------------------------------------------------------------------
  # get total inputs of polymerization
  # ---------------------------------------------------------------------------
  polymerization <- df %>% filter(type=="plastics") %>% group_by(.data$output) %>%
    summarize(sum_inputs = sum(.data$coef)) %>% rename("polymer"="output")
  polymerization_magclass <- as.magpie(polymerization)

  # ---------------------------------------------------------------------------
  # calculate total HVC input for each polymer from HVC inputs of all intermediate steps
  # ---------------------------------------------------------------------------
  get_HVC_inputs <- function(product, data, multiplier = 1) {
    rows <- data %>% filter(output == product)
    # If product has no further decomposition, it is a HVC
    if (nrow(rows) == 0) {
      return(tibble::tibble(input = product, tonnes = multiplier))
    }
    # Otherwise recurse through all its inputs
    purrr::map_dfr(seq_len(nrow(rows)), function(i) {
      get_HVC_inputs(
        product = rows$input[i],
        data = data,
        multiplier = multiplier * rows$coef[i]
      )
    })
  }

  # Apply to all polymers
  HVC_input <- purrr::map_dfr(polymerization$polymer, function(p) {
    get_HVC_inputs(p, df) %>%
      group_by(input) %>%
      summarise(tonnes = sum(tonnes), .groups = "drop") %>%
      mutate(polymer = p) %>%
      select(polymer, input, tonnes)
  })

  # get carbon content of HVC
  carbon_content <- calcOutput("PlCarbonContent", subtype="HVC", aggregate=FALSE) %>% as.data.frame() %>%
    select(-"Cell",-"Region",-"Year") %>% rename("element" = "Data2")

  # merge with HVC input and calculate total HVC input by element
  HVC_input <- merge(HVC_input, carbon_content, by.x="input", by.y="Data1") %>%
    mutate(input_by_element = .data$tonnes*.data$Value, hvc_type = case_when(.data$input=="butadiene" ~ "C4", .default="HVC w/o C4")) %>%
    group_by(.data$polymer, .data$element, .data$hvc_type) %>%
    summarize(HVC_input = sum(input_by_element))
  HVC_input_magclass <- as.magpie(HVC_input)

  # ---------------------------------------------------------------------------
  # map polymers to REMIND-MFA polymers using production weights for ABS&SAN mapping to ABS_ASA_SAN
  # ---------------------------------------------------------------------------
  polymer_map <- toolGetMapping("polymermappingLeviCullen.csv", type = "sectoral", where = "mrmfa")
  production_volumes <- readSource("LeviCullen", subtype = "Production", convert = FALSE)
  weights <- mselect(production_volumes, stage = "total production", type="plastics", to=plastics$output) %>% collapseDim()
  HVC_input_mapped <- HVC_input_magclass %>%
    toolAggregate(rel = polymer_map, dim = 3.1, from = "Source", to = "Target", weight = weights, wdim = 3.1)
  polymerization_mapped <- polymerization_magclass %>%
    toolAggregate(rel = polymer_map, dim = 3.1, from = "Source", to = "Target", weight = weights, wdim = 3.1)

  # ---------------------------------------------------------------------------
  # fill data for Fibres, Bioplastics and Other with assumptions:
  # for Fibres, assume same input as for PET, since 85% of global Fibre production is polyester
  # for Bioplastics and Other, regress HVC inputs by carbon content and assume typical polymerization yield
  # according to Levi and Cullen 2018 (total input = 1.02)
  # ---------------------------------------------------------------------------

  # get carbon content of plastics (dim 3.1: polymer, dim 3.2: element C / Other Elements)
  carbon_content_pl <- calcOutput("PlCarbonContent", subtype = "plastics", aggregate = FALSE) %>%
    mselect(Element = "C") %>% collapseDim()  # keep only C content, result: (polymer)

  # --- Fibres: copy HVC input and polymerization yield from PET ---
  fibres_HVC   <- HVC_input_mapped[, , "PET"]
  getItems(fibres_HVC,   dim = 3.1) <- "Fibres"
  fibres_polym <- polymerization_mapped[, , "PET"]
  getItems(fibres_polym, dim = 3.1) <- "Fibres"

  # --- Bioplastics & Others: linear regression of HVC input on carbon content ---
  # build a dataframe of known polymer carbon contents and their HVC inputs per (element, HVC type)
  known_polymers <- getItems(HVC_input_mapped, dim = 3.1)
  cc_known <- as.data.frame(mselect(carbon_content_pl, Polymer=known_polymers)) %>%
    select("Data1", "Value") %>%
    rename(polymer = "Data1", carbon = "Value")

  hvc_known <- as.data.frame(HVC_input_mapped) %>%
    select(-"Cell", -"Region", -"Year") %>%
    rename(polymer = "Data1", element = "Data2", hvc_type = "Data3", hvc_input = "Value")

  reg_data <- merge(hvc_known, cc_known, by = "polymer")

  # for each element, fit lm on HVC w/o C4 only and predict; C4 input is assumed zero
  missing_polymers <- c("BioPlastics", "Others")
  cc_missing <- as.data.frame(mselect(carbon_content_pl, Polymer=missing_polymers)) %>%
    select("Data1", "Value") %>%
    rename(polymer = "Data1", carbon = "Value")

  predicted <- reg_data %>%
    filter(.data$hvc_type == "HVC w/o C4") %>%
    group_by(.data$element) %>%
    tidyr::nest() %>%
    mutate(
      model = purrr::map(.data$data, ~ lm(hvc_input ~ carbon, data = .x)),
      pred  = purrr::map(.data$model, ~ predict(.x, newdata = cc_missing))
    ) %>%
    tidyr::unnest(pred) %>%
    mutate(polymer = rep(missing_polymers, dplyr::n() / length(missing_polymers)),
           hvc_type = "HVC w/o C4") %>%
    select("polymer", "element", "hvc_type", hvc_input = "pred")

  missing_HVC <- as.magpie(predicted %>% select(polymer, element, hvc_type, hvc_input))
  missing_HVC <- add_columns(missing_HVC, addnm = "C4", dim = "hvc_type", fill = 0)
  getItems(missing_HVC, dim=1) <- NULL

  # polymerization yield of 98% for Bioplastics and Others
  missing_polym <- new.magpie(names = missing_polymers,
                              sets = c("region","year","polymer"), fill = 1.02)
  getItems(missing_polym, dim=1) <- NULL

  # --- combine all polymers ---
  HVC_input_full   <- mbind(HVC_input_mapped, fibres_HVC, missing_HVC)
  polymerization_full <- mbind(polymerization_mapped, fibres_polym, missing_polym)
  test <- dimSums(HVC_input_full, dim=3.2) %>% as.data.frame(rev=3)

  # get HVC input ratio
  HVC_input_ratio = HVC_input_full/polymerization_full
  test <- dimSums(HVC_input_ratio, dim=3.2) %>% as.data.frame(rev=3)

  # ---------------------------------------------------------------------------
  # return data according to specified subtype
  # ---------------------------------------------------------------------------

  if (subtype == "polymerization_yield") {
    x <- polymerization_full
    note <- "dimensions: (Material,value)"
  } else if (subtype == "HVC_input_ratio"){
    x <- mselect(HVC_input_ratio, hvc_type = "HVC w/o C4") %>% collapseDim()
    note <- "dimensions: (Material,Element,value)"
  } else if (subtype == "C4_input_ratio"){
    x <- mselect(HVC_input_ratio, hvc_type = "C4") %>% collapseDim()
    note <- "dimensions: (Material,Element,value)"
  } else {
    stop("Invalid subtype. Choose either 'polymerization_yield' or 'HVC_input_ratio'.")
  }
  description <- paste(
    subtype,
    " of plastics by polymer. ",
    "Data derived from Levi and Cullen 2018 https://doi.org/10.1021/acs.est.7b04573 for available polymers; ",
    "for PUR, Fibres, Others and Bioplastics based on assumptions regarding their composition."
  )
  output <- list(
    x = x,
    weight = NULL,
    unit = "ratios",
    description = description,
    isocountries = FALSE,
    note = note
  )
  return(output)
}
