##' Populate the biomass table with updated biomass estimates and provenance
##' information.
##'
##' @title Populate the biomass table
##'
##' @param allometry_data The allometry matrix
##' @param equation_bank Allometric equation bank
##' @param biomass_data The biomass table
##' @param species_data Species IDs with taxonomy
##' @param category_data The category lookup table
##' @param closest_relatives The table of closest relatives
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
populate_biomass_table <- function(allometry_data, equation_bank, biomass_data,
                                   species_data, category_data, closest_relatives){
  ## Remove unneeded columns from the allometry matrix
  allometry <- allometry_data %>%
    dplyr::select(-size_category, -instar_number, -number_of_individuals)

  ## Create allometry matrix with species ids
  allometry_species <- allometry %>%
    dplyr::left_join(., species_data)

  ## Create equation bank with species ids
  equation_species <- equation_bank %>%
    dplyr::left_join(., species_data)

  ## A clean data frame in which to add new biomass information
  ## The data are joined with the table of category-length estimates, and the
  ## table of closest relatives; once joined to category_data, the size_category
  ## column is no longer needed.
  biomass_clean <- biomass_data %>%
    dplyr::left_join(., category_data) %>%
    dplyr::left_join(., closest_relatives) %>%
    dplyr::select(-size_category, -category_range)

  # Calculate biomasses
  meas_ids    <- unique(biomass_clean$measurement_id)
  biomass_all <- generate_biomass(biomass_clean, meas_ids,
                                  "length.raw", "exact",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "length.interpolate", "exact",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "category.raw", "exact",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "category.interpolate", "exact",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "length.raw", "related",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "length.interpolate", "related",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "category.raw", "related",
                                  allometry, equation_bank)
  meas_ids    <- unique(biomass_all$measurement_id)
  biomass_all <- generate_biomass(biomass_all, meas_ids,
                                  "category.interpolate", "related",
                                  allometry, equation_bank)

  biomass_clean <- biomass_clean %>%
    dplyr::filter(!(measurement_id %in% meas_ids))

  # Compute median biomasses for multiple relatives
  biomass_med <- generate_median_biomass(biomass_clean, allometry_species, equation_species)

  # Join everything
  biomass_all <- biomass_all %>%
    dplyr::full_join(., biomass_med) %>%
    dplyr::select(species_id, measurement_id, bwg_name, stage,
                  length_measured_as, length_mm, length_est_mm,
                  biomass_type, biomass_mg, biomass_ci_upr, biomass_ci_lwr,
                  provenance, provenance_species,
                  closest_relative, num_relatives, shared_taxon,
                  r_squared, sample_size, intercept, slope)

  return(biomass_all)
}

##' Generate biomass calculation for given measurement. Provenance information
##' is added at this time.
##'
##' @title Generate biomass
##'
##' @param biomass_data The biomass data, with all current biomass estimates
##' @param meas_ids IDs of measurements that already have biomass
##' @param prov Provenance of the biomass measurement
##' @param prov_species Provenance determined for the species in question, or
##'   the closest related specices
##' @param allometry_data The allometry matrix
##' @param equation_bank The equation bank
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
generate_biomass <- function(biomass_data, meas_ids, prov, prov_species,
                             allometry_data, equation_bank){
  # Set measurement and type based on provenance input
  provenance <- stringr::str_split(prov, "\\.", n = 2)
  measurement <- provenance[[1]][1]
  type        <- provenance[[1]][2]

  # Define variables based on prov and prov_species:

  # Use lengths or length estimates?
  by_length <- switch(measurement,
                      length   = "length_mm",
                      category = "length_est_mm")

  # Use the bwg_name of the target species, or the name of the closest_relative?
  by_species <- switch(prov_species,
                       exact   = "bwg_name",
                       related = "closest_relative")

  # Filter by length_mm or length_est_mm
  biomass_data <- biomass_data %>%
    dplyr::filter(!(measurement_id %in% meas_ids)) %>%
    dplyr::filter(!is.na(biomass_data[, by_length]))

  # Join to allometry matrix or equation bank and interpolate or not
  if(type == "raw"){
    by_vars        <- c("bwg_name", "stage", "length_mm")
    names(by_vars) <- c(by_species, "stage", by_length)

    biomass_data <- biomass_data %>%
      dplyr::inner_join(., allometry_data, by = by_vars)

  } else if(type == "interpolate"){
    by_vars        <- c("bwg_name", "stage")
    names(by_vars) <- c(by_species, "stage")

    biomass_data <- biomass_data %>%
      dplyr::inner_join(., equation_bank, by = by_vars) %>%
      interpolate_biomass(., measurement)

  } else stop("Unrecognized provenance type.")

  # Add provenance info and update biomass_clean
  biomass_data <- biomass_data %>%
    dplyr::mutate(provenance         = prov,
                  provenance_species = prov_species)

  # Join to the input biomass_data unless there is nothing to join
  if(nrow(biomass_data) == 0){
    return(biomass_data)
  } else if(prov == "length.raw" & prov_species == "exact"){
    return(biomass_data)
  } else {
    biomass_data <- full_join(biomass_data, biomass_data)
    return(biomass_data)
  }
}

##' Generate median biomass.
##'
##' @title Generate median biomass
##'
##' @param biomass_data The cleaned biomass table
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
generate_median_biomass <- function(biomass_data, allometry_species, equation_species){
  # Filter out species with multiple closest relatives and no length_mm or
  # length_est_mm
  biomass_med <- biomass_data %>%
    dplyr::filter(num_relatives > 1) %>%
    dplyr::filter(!is.na(length_mm) | !is.na(length_est_mm))

  for(i in 1:nrow(biomass_med)){
    # The name of the shared taxon
    shared_taxon <- biomass_med$shared_taxon[i]
    # The id of the shared taxon
    taxon_name   <- biomass_med[i, shared_taxon]

    # Provenance "length" or "category"
    if(!is.na(biomass_med$length_mm[i])) {
      prov_measurement <- "length"
      by_length        <- "length_mm"
    } else if(!is.na(biomass_med$length_est_mm[i])) {
      prov_measurement <- "category"
      by_length        <- "length_est_mm"
    } else stop("No length available.")

    # Selection for provenance "raw"
    # Select all species in the allometry matrix with the shared taxon
    shared <- allometry_species %>%
      dplyr::filter(allometry_species[shared_taxon] == taxon_name) %>%
      dplyr::filter(length_mm == biomass_med[i, by_length]) %>%
      filter_by_biomass_type()

    ## Raw vs. interpolate
    if(nrow(shared) > 0){
      ## Use the length to get a raw biomass
      prov_type <- "raw"
      median_select_vars <- c("biomass_type", "biomass_mg")
    } else {
      ## Use the length to interpolate biomass using the equation bank
      prov_type <- "interpolate"

      # Select all species in the equation bank with the shared taxon
      shared <- equation_species %>%
        dplyr::filter(equation_species[shared_taxon] == taxon_name) %>%
        filter_by_biomass_type() %>%
        dplyr::mutate(length_mm = biomass_med[i, by_length]) %>%
        interpolate_biomass(., prov_measurement)

      median_select_vars <- c("biomass_type", "biomass_mg", "r_squared",
                              "sample_size", "slope", "intercept",
                              "biomass_ci_upr", "biomass_ci_lwr")

      if(nrow(shared) == 0){
        stop("No shared species.")
      }
    }

    # Select the species with median biomass; remove extra cols for joining
    median_species <- shared %>%
      get_median_species() %>%
      dplyr::select(!!median_select_vars) %>%
      dplyr::mutate(measurement_id = biomass_med$measurement_id[i])

    # Add biomass measurements
    biomass_med$closest_relative[i] <- paste(shared$bwg_name, sep="", collapse = ";")
    biomass_med$provenance[i] <- paste(prov_measurement, prov_type, sep = ".")
    biomass_med$provenance_species[i] <- "related"

    # Create data frame with all median species
    if(i == 1){
      all_medians <- median_species
    } else {
      all_medians <- full_join(all_medians, median_species)
    }
  }

  # Join medians with biomass table
  biomass_meds <- biomass_med %>%
    dplyr::full_join(., all_medians) %>%
    dplyr::filter(!is.na(biomass_mg))

  return(biomass_meds)
}

##' Filter based on whether the measurement is for dry or wet biomass.
##'
##' @title Filter by biomass type
##'
##' @param biomass_data The cleaned biomass table
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
filter_by_biomass_type <- function(biomass_data){
  # Dry or wet biomass?
  if(any(biomass_data$biomass_type == "dry")){
    biomass_selector <- "dry"
  } else {
    biomass_selector <- "wet"
  }

  biomass_data <- biomass_data %>%
    dplyr::filter(biomass_type == biomass_selector)

  return(biomass_data)
}

##' Get the identity of species with median biomass
##'
##' @title Get median species
##'
##' @param shared_species The vector of shared species
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
get_median_species <- function(shared_species){
  median_species <- shared_species %>%
    dplyr::mutate(median_biomass = median(biomass_mg)) %>%
    dplyr::mutate(biomass_diff   = abs(biomass_mg - median_biomass)) %>%
    dplyr::filter(biomass_diff == min(biomass_diff))

  # If there are more than one species equally close to the median, select the
  # one with smaller biomass
  if(nrow(median_species) > 1){
    median_species <- median_species %>%
      dplyr::filter(biomass_mg == min(biomass_mg))

    # Select the first row if there are still more than one median species
    median_species <- median_species[1,]
  }

  # There should be exactly one median species
  if(nrow(median_species) != 1){
    stop("Something is wrong with determining median species.")
  }

  # Select the species with biomass closest to the median
  shared_species <- shared_species %>%
    dplyr::filter(species_id == median_species$species_id)

  return(shared_species)
}

##' Get the identity of species with median biomass
##'
##' @title Interpolate biomass
##'
##' @param biomass_data The biomass table
##' @param type Interpolate using "length" or "category"
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
interpolate_biomass <- function(biomass_data, type){
  biomass_data <- biomass_data %>%
    dplyr::mutate(biomass_mg = NA, biomass_ci_upr = NA, biomass_ci_lwr = NA)

  # Return if there is no data
  if(nrow(biomass_data) == 0){
    return(biomass_data)
  }

  # Calculate biomass using the regression
  for(i in 1:nrow(biomass_data)){

    if(type == "length"){
      new <- data.frame(length_mm = biomass_data$length_mm[i])
    } else if(type == "category"){
      new <- data.frame(length_mm = biomass_data$length_est_mm[i])
    } else {
      stop("Incorrect type specified. Use 'length' or 'category'.")
    }

    biomass <- predict(biomass_data$fit[[i]], newdata = new,
                       interval = "confidence")

    biomass_data$biomass_mg[i]     <- 10^(biomass[1,"fit"])
    biomass_data$biomass_ci_upr[i] <- 10^(biomass[1,"upr"])
    biomass_data$biomass_ci_lwr[i] <- 10^(biomass[1,"lwr"])
  }

  biomass_data <- biomass_data %>%
    dplyr::select(-fit)

  return(biomass_data)
}

