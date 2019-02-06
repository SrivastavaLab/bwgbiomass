##' Create the cleaned up biomass table by importing the existing biomass
##' data and removing existing biomass information.
##'
##' @title Create the cleaned up biomass table
##'
##' @param species_ids A table of species_ids and corresponding taxonomy info
##' created by using the function "get_species_ids()" in this package. Default
##' value of "NULL".
##'
##' @importFrom dplyr %>%
##' @export
##'
create_biomass_table <- function(species_ids = NULL){
  # Create the data frame of species IDs if it is missing
  if(!(exists("species_ids"))){
    species_ids <- get_species_ids()
  }

  biomass_table <- obtain_biomass_table(species_ids) %>%
    clean_lengths() %>%
    correct_stages() %>%
    correct_size_categories() %>%
    all_ostracods_adults()

  return(biomass_table)
}

##' Obtain the original biomass table from the saved package data. Clean
##' the table for further use. The original table is called "biomass_original"
##' and is stored as system data (sysdata).
##'
##' @title Obtain original biomass table
##'
##' @param species_info A table of species_ids and corresponding taxonomy info
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
obtain_biomass_table <- function(species_info){
  # Merge biomass table with species names from bwgdb
  # Add the species names to the table
  biomass_named <-  merge(biomass_original, species_info)

  # Remove existing biomass values

  # Clear the biomass table for adding new info. and split the category_value
  biomass_clean <- biomass_named %>%
    dplyr::select(species_id, measurement_id, bwg_name, category_range,
                  category_value, range_min, range_max) %>%
    dplyr::mutate(stage         = stringr::str_split_fixed(.$category_value, "_", 2)[,1],
                  size_category = stringr::str_split_fixed(.$category_value, "_", 2)[,2]) %>%
    dplyr::mutate(length_mm = NA) %>%
    dplyr::select(-category_value)

  # Set blank cells to NA
  biomass_clean[biomass_clean$stage == "", "stage"] <- NA

  return(biomass_clean)
}

##' Takes strings that should be lengths, cleans them up, and converts them
##' accordingly.
##'
##' @title Clean lengths
##'
##' @param biomass_table The biomass table
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
clean_lengths <- function(biomass_table){

  for(i in 1:nrow(biomass_table)){
    size <- biomass_table$size_category[i]
    size_num <- suppressWarnings(as.numeric(size))

    if(!is.na(size_num)){
      # Move numerics directly over
      biomass_table$length_mm[i] <- size_num
    } else if(stringr::str_detect(size, "mm")){
      size <- size %>%
        stringr::str_replace("mm", "")

      if(stringr::str_detect(size, "_")){
        size <- size %>%
          stringr::str_split("_")

        biomass_table$length_mm[i] <- mean(as.numeric(size[[1]]))
      } else {
        size <- as.numeric(size)

        biomass_table$length_mm[i] <- size
      }
    } else if(biomass_table$category_range[i] == "range"){
      biomass_table$length_mm[i] <-
        (biomass_table$range_min[i] + biomass_table$range_max[i]) / 2
    }
  }

  biomass_table <- biomass_table %>%
    dplyr::select(-range_min, -range_max)

  return(biomass_table)
}

##' Correct stage errors. Stage "default" is converted to "larva" to match the
##' allometry matrices. Species with stage "larva" and size category NA are
##' given size category "average". Some rows have "stages" that are actually a
##' size category. For example, the stage is listed as "small" instead of
##' "larva".  In these rows, the erroneous stage is moved into the
##' "size_catgory" column, and "stage" is assumed to be "larva".
##'
##' @title Correct stages
##'
##' @param biomass_data The biomass table
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
##'
correct_stages <- function(biomass_data){
  biomass_table <- biomass_data

  # Convert stage "default" to "larva" to match the allometry matrices
  biomass_table[which(biomass_table$stage == "default"), "stage"] <- "larva"

  # Species with stage "larva" and size_category NA are given size_category
  # "average"
  biomass_table[which(biomass_table$stage == "larva" &
                        is.na(biomass_table$size_category)), "size_category"] <- "average"

  # Convert blank size_category or stage to NA
  biomass_table[which(biomass_table$stage == ""),"stage"] <- NA
  biomass_table[which(biomass_table$size_category == ""),"size_category"] <- NA


  # Correct stage errors
  stages <- c("pupa", "larva", "adult")

  stage_errors <- which(!(biomass_table$stage %in% stages))

  for(i in stage_errors){
    biomass_table$size_category[i] <- biomass_table$stage[i]
    biomass_table$stage[i] <- "larva"
  }

  return(biomass_table)
}

##' Correct NA size categories - generally replacing NA or "regular" with
##' "average".
##'
##' @title Correct size categories
##'
##' @param biomass_data The biomass table
##'
##' @keywords internal
##' @export
##'
correct_size_categories <- function(biomass_data){
  # If the stage is "larva", "size_category" is NA and "length_mm" is NA, it is
  # size_category is assumed to be an average sized larva, and is changed to
  # "average".
  size_NA <- which(is.na(biomass_data$size_category) &
                     is.na(biomass_data$length_mm) &
                     biomass_data$stage == "larva")

  biomass_data[size_NA, "size_category"] <- "average"

  # # Change size "regular" to "average"
  # size_regular <- which(biomass_data$size_category == "regular")
  # biomass_data[size_regular, "size_category"] <- "average"
  #
  # # Change size L to "large"
  # size_large <- which(biomass_data$size_category == "L")
  # biomass_data[size_large, "size_category"] <- "large"
  #
  # # Change size "unkown" or "unknown" to "average"
  # size_unk <- which(biomass_data$size_category == "unkown" |
  #                   biomass_data$size_category == "unknown")
  # biomass_data[size_unk, "size_category"] <- "average"

  return(biomass_data)
}

##' Set stage for all Ostracoda to "adult"
##'
##' @title All ostracods adults
##'
##' @param biomass_data The biomass table
##'
##' @keywords internal
##' @export
##'
all_ostracods_adults <- function(biomass_data){
  # Convert all Ostracods to adults

  biomass_table <- biomass_data

  ostracoda <- stringr::str_which(biomass_table$bwg_name, "Ostracoda")

  biomass_table[ostracoda, "stage"] <- "adult"

  return(biomass_table)
}
