##' Create a table of the closest taxonomic relatives to any given species.
##'
##' @title Get closest relatives
##'
##' @param biomass_data The biomass table
##'
##' @param allometry_data The cleaned up allometry data
##'
##' @param species_data Species ID data with taxonomy attached
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
get_closest_relatives <- function(biomass_data, allometry_data, species_data){
  # Get the list of species that are missing from the allometry matrix
  missing <- which(!(biomass_data$bwg_name %in% allometry_data$bwg_name))

  # Get the list of species in the allometry matrix and add in taxonomic info
  allometry_sp <- allometry_data %>%
    dplyr::distinct(bwg_name) %>%
    dplyr::left_join(., species_data)

  # Create a data frame to hold missing species and closest relative
  closest_relatives <- biomass_data[missing, ] %>%
    dplyr::distinct(bwg_name, .keep_all = TRUE) %>%
    dplyr::select(species_id, bwg_name) %>%
    dplyr::left_join(., species_data) %>%
    dplyr::mutate(closest_relative = NA, num_relatives = NA, shared_taxon = NA)

  # Vector of column names for taxon columns
  taxa <- c("domain", "kingdom", "phylum", "subphylum", "class", "subclass",
            "ord", "subord", "family", "subfamily", "tribe", "genus", "species",
            "subspecies")

  # Find nearest taxonomic relative(s)
  for(i in 1:nrow(closest_relatives)){
    # list of all alometry matrix species before filtering
    sp_list       <- allometry_sp
    sp_list$level <- "domain"
    # indices of taxon columns, NA filtered out
    taxon_cols <- which(names(closest_relatives) %in% taxa & !is.na(closest_relatives[i,]))

    for(j in taxon_cols){
      # Find the matching taxon column in the sp_list
      col_index <- which(names(sp_list) == names(closest_relatives[j]))
      # Get the name of the current taxon in the closest_relatives data
      taxon <- closest_relatives[i,j]

      # Filter the species list, removing taxa that don't match the missing
      # species taxon at the current level
      sp_list_filtered <- sp_list %>%
        dplyr::filter(sp_list[,col_index] == taxon)

      # If the filtered list is empty, enter the info from previous taxonomic
      # level
      if(nrow(sp_list_filtered) == 0){

        if(nrow(sp_list) == 1){
          closest_relatives[i,]$closest_relative <- sp_list$bwg_name
        } else {
          closest_relatives[i,]$closest_relative <- NA
        }

        closest_relatives[i,]$num_relatives    <- nrow(sp_list)
        closest_relatives[i,]$shared_taxon     <- unique(sp_list$level)

        j <- max(taxon_cols)
        break
      }

      sp_list <- sp_list_filtered
      sp_list$level <- names(closest_relatives[j])

      # For last column, add closest relative info no matter what
      if(j == max(taxon_cols)){
        if(nrow(sp_list) == 1){
          closest_relatives[i,]$closest_relative <- sp_list$bwg_name
        } else {
          closest_relatives[i,]$closest_relative <- NA
        }

        closest_relatives[i,]$num_relatives    <- nrow(sp_list)
        closest_relatives[i,]$shared_taxon     <- unique(sp_list$level)
      }
    }
  }

  return(closest_relatives)
}



