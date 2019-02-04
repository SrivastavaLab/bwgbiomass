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
  # Convert original measurements to mg
  for(i in 1:nrow(biomass_original)){
    if(biomass_original$unit[i] == "g"){
      biomass_original$biomass[i] <- biomass_original$biomass[i] * 1000
    }
  }

  # Select relevant columns from the original data
  biomass_original <- biomass_original %>%
    dplyr::select(measurement_id, biomass, dry_wet) %>%
    dplyr::rename(biomass_orig_mg = biomass, biomass_type = dry_wet)

  # Select relevant columns from the new data
  biomass_new <- biomass_data %>%
    dplyr::select(bwg_name, measurement_id, provenance, provenance_species,
                  biomass_mg, biomass_type) %>%
    dplyr::rename(biomass_new_mg = biomass_mg)

  biomass_all <- dplyr::left_join(biomass_new, biomass_original) %>%
    dplyr::filter(!(is.na(biomass_orig_mg)))

  biomass_all$new_over_old <- with(biomass_all, biomass_new_mg / biomass_orig_mg)

  return(biomass_all)
}
