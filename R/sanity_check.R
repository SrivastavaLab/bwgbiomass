##' Compares the calculated biomass measurements with those found in the
##' original biomass table.
##'
##' @title Sanity check biomass measurements
##'
##' @param biomass_data A table of biomass estimates. Must include the
##' measurement_id.
##'
##' @return A data.frame comparing new biomass estimates with the original
##'
##' @export
sanity_check <- function(biomass_data){
  ## Clean up the original biomass measurements
  # Convert original measurements to mg
  for(i in 1:nrow(biomass_original)){
    if(biomass_original$unit[i] == "g"){
      biomass_original$biomass[i] <- biomass_original$biomass[i] * 1000
    }
  }

  # Add length information
  biomass_orig_clean <- biomass_original %>%
    dplyr::rename(biomass_mg = biomass, biomass_type = dry_wet) %>%
    dplyr::mutate(stage         = stringr::str_split_fixed(.$category_value, "_", 2)[,1],
                  size_category = stringr::str_split_fixed(.$category_value, "_", 2)[,2]) %>%
    dplyr::mutate(length_mm = NA) %>%
    dplyr::select(-category_value)

  # Set blank cells to NA
  biomass_orig_clean[biomass_orig_clean$stage == "", "stage"] <- NA

  # Select relevant columns from the original data
  biomass_orig_final <- biomass_orig_clean %>%
    clean_lengths() %>%
    correct_stages() %>%
    correct_size_categories() %>%
    all_ostracods_adults()

  biomass_orig_final <- biomass_orig_final %>%
    dplyr::select(species_id, measurement_id, biomass_mg, biomass_type, stage,
                  length_mm) %>%
    dplyr::rename(biomass_orig_mg = biomass_mg)

  # Select relevant columns from the new data
  biomass_new <- biomass_data %>%
    dplyr::select(bwg_name, measurement_id, provenance, provenance_species,
                  biomass_mg, biomass_type, biomass_ci_upr, biomass_ci_lwr, r_squared) %>%
    dplyr::rename(biomass_new_mg = biomass_mg)

  biomass_all <- dplyr::left_join(biomass_new, biomass_orig_final) %>%
    dplyr::filter(!(is.na(biomass_orig_mg)))

  biomass_all$new_over_old <- with(biomass_all, biomass_new_mg / biomass_orig_mg)

  # Rearrange the data frame
  biomass_all <- biomass_all %>%
    dplyr::select(measurement_id, species_id, bwg_name, stage, length_mm,
                  provenance, provenance_species, biomass_type, biomass_orig_mg,
                  biomass_new_mg, new_over_old, biomass_ci_upr, biomass_ci_lwr,
                  r_squared)

  return(biomass_all)
}
