##' Add a placeholder for species missing a BWG name. Makes it possible to
##' group by bwg_name even when one wasn't given.
##'
##' @title Add missing names
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
add_missing_bwg_names <- function(allometry_data){
  # Add a name to NA bwg names
  missing_names <- allometry_data %>%
    dplyr::filter(is.na(bwg_name)) %>%
    dplyr::mutate(id = dplyr::group_indices(., name)) %>%
    dplyr::mutate(bwg_name = paste("MISSING", id, sep="_")) %>%
    dplyr::select(-id)

  # Get all the bwg_names
  named_data <- allometry_data %>%
    dplyr::filter(!is.na(bwg_name)) %>%
    dplyr::full_join(missing_names)

  # Make sure merge was correct by checking number of rows
  if(nrow(named_data) != nrow(allometry_data)){
    stop("Merge error.  Num rows in named_data not equal to allometry_data.")
  }

  return(named_data)
}

##' Calculate biomass. When multiple mass measurements per species-length are
##' included, the weighted average is taken.
##'
##' @title Calculate biomass
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
calculate_biomass <- function(allometry_data){
  allometry_matrix <- allometry_data %>%
    dplyr::mutate(biomass_wet_mg = wet_mass_mg / number_of_individuals,
                  biomass_dry_mg = dry_mass_mg / number_of_individuals) %>%
    dplyr::group_by(bwg_name, length_mm, stage, size_category, instar_number) %>%
    dplyr::mutate(biomass_wet_mg = mean(biomass_wet_mg),
                  biomass_dry_mg = mean(biomass_dry_mg)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-wet_mass_mg, -dry_mass_mg) %>%
    unique() %>%
    tidyr::gather(biomass_wet_mg:biomass_dry_mg,
                  key = biomass_type, value = biomass_mg)

  allometry_matrix[allometry_matrix$biomass_type == "biomass_wet_mg",
                   "biomass_type"] <- "wet"
  allometry_matrix[allometry_matrix$biomass_type == "biomass_dry_mg",
                   "biomass_type"] <- "dry"

  return(allometry_matrix)
}

##' Standardize developmental stages. Possible stages are: "larva", "pupa",
##' "adult" or "NA".
##'
##' @title Standardize developmental stage
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @return The data with stages standardized.
##'
##' @keywords internal
##' @export
standardize_stages <- function(allometry_data){
  allometry_matrix <- allometry_data

  stages <- c("larva", "pupa", "adult", NA)

  # Change "larvae" to "larva"
  allometry_matrix[which(allometry_matrix$stage == "larvae"), "stage"] <- "larva"

  # Error thrown if unknown developmental stage is used
  if(!all(allometry_matrix$stage %in% stages)){
    stop("Unexpected developmental stage.  Expecting 'larva', 'pupa', 'adult', or NA.")
  }

  return(allometry_matrix)
}

##' Remove rows with no biomass.
##'
##' @title Remove rows with no biomass
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @return Data with missing biomasses filtered out.
##'
##' @keywords internal
##' @export
remove_missing_biomass <- function(allometry_data){
  allometry_matrix <- dplyr::filter(allometry_data, !(is.na(biomass_mg)))

  return(allometry_matrix)
}
