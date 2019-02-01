##' Get species taxonomy information from BWGdb to go with the species ID.
##'
##' @title Get Species IDs
##'
##' @keywords internal
##' @export
get_species_ids <- function(){
  # Access the database data using fwdata
  species_ids <- fwdata::fw_data(fwdata::fw_version_current())$traits %>%
    dplyr::select(species_id:subspecies)

  # Convert character to integer
  species_ids$species_id <- as.integer(species_ids$species_id)

  # Fix errors in domain name
  domain_errors <- which(species_ids$domain %in% c("Eukarya", "Insecta", "Animalia"))
  species_ids[domain_errors, "domain"] <- "Eukaryota"

  return(species_ids)
}
