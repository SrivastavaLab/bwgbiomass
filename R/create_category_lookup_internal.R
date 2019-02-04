##' Select the relevant columns for the category lookup table. length_mm is
##' renamed to length_est_mm as the lengths are estimates for category
##' definitions.
##'
##' @title Select category cols
##'
##' @param category_lookup The category lookup table.
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
select_category_cols <- function(allometry_data){
  category_lookup <- allometry_data %>%
    dplyr::rename(length_est_mm = length_mm) %>%
    dplyr::select(bwg_name, stage, size_category, length_est_mm)

  # Standardize stage
  category_lookup[category_lookup$stage == "larvae", "stage"] <- "larva"

  return(category_lookup)
}

##' Standardize the size categories in the category lookup table.
##'
##' @title Standardize size categories.
##'
##' @param category_lookup The category lookup table.
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
standardize_size_categories <- function(category_lookup){
  category_lookup$size_category <- category_lookup$size_category %>%
    dplyr::recode("very large" = "xlarge",
                  "large med"  = "medium_large",
                  "small med"  = "small_medium",
                  "extra tiny" = "xtiny")

  return(category_lookup)
}
