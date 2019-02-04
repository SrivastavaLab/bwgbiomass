##' Generate biomass estimates
##' (\url{https://github.com/SrivastavaLab/bwgbiomass/}) from allometry data
##'  (\url{https://github.com/SrivastavaLab/allometrydata/})
##' @title Generate biomass estimates
##'
##' @param data_dir A local data directory in which to save the biomass data
##'
##' @param version Optional data version number
##'
##' @import fwdata
##' @importFrom dplyr %>%
##' @export
gen_biomass <- function(data_dir = "_data", version = NULL){

  # I/O Variables
  # Biomass filename generated from allometry data version and biomass data
  # version, if provided.
  allometry_version <- allometry_version_current()

  if(is.null(version)){
    biomass_filename <- paste(
      data_dir, "/biomass_", format(Sys.time(),"%Y%m%d"), "_", allometry_version, ".rds",
      sep = ""
    )
  } else {
    biomass_filename <- paste(
      data_dir, "/biomass_v.", version, "_", allometry_version, ".rds",
      sep = ""
    )
  }

  # The raw allometry data with placeholder for missing names
  allometry_named <- allometry() %>%
    add_missing_bwg_names()

  # The clean allometry matrix
  allometry_matrix <- allometry_named %>%
    calculate_biomass() %>%
    filter_type() %>%
    standardize_stages() %>%
    remove_missing_biomass()

  # Taxonomy info to go with species IDs
  species_ids <- get_species_ids()

  # Equation bank for calculating biomass
  equation_bank <- allometry_matrix %>%
    clean_allometry_matrix() %>%
    run_linear_models() %>%
    obtain_summary_data()

  # Category lookup table
  category_lookup <- allometry_named %>%
    filter_type(measurement = FALSE) %>%
    select_category_cols()

  # Clean the original biomass table
  biomass_table <- generate_biomass_table(species_ids) %>%
    clean_lengths() %>%
    correct_stages() %>%
    correct_size_categories() %>%
    all_ostracods_adults()

  # Get the table of closest relatives
  closest_relatives <- get_closest_relatives(
    biomass_data   = biomass_table,
    allometry_data = allometry_matrix,
    species_data   = species_ids
    )

  # Populate the biomass table
  biomass_estimates <- populate_biomass_table(
    allometry_data    = allometry_matrix,
    equation_bank     = equation_bank,
    biomass_data      = biomass_table,
    species_data      = species_ids,
    category_data     = category_lookup,
    closest_relatives = closest_relatives
    )

  # Save the new data in the local data_dir for uploading
  saveRDS(biomass_estimates, file = biomass_filename)

  return(biomass_estimates)
}
