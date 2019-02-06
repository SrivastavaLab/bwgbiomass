##' Create the allometric equation bank
##'
##' @title Create equation bank
##'
##' @param allometry_matrix The allometry matrix as created by this package
##'  using the "create_allometry_matrix()" function. Default value is NULL. If
##'  left as NULL, the function will start by creating the allometry matrix.
##'
##'  @return A data frame containing the equation bank
##'
##' @importFrom dplyr %>%
##' @export
create_equation_bank <- function(allometry_matrix = NULL){
  # Check for the allometry matrix and create it if it doesn't exist
  if(is.null(allometry_matrix)){
    allometry_matrix <- create_allometry_matrix()
  }

  equation_bank <- allometry_matrix %>%
    clean_allometry_matrix() %>%
    run_linear_models() %>%
    obtain_summary_data()

  return(equation_bank)
}

##' Remove rows with missing mass or length
##'
##' @title Clean allometry matrix
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
clean_allometry_matrix <- function(allometry_data){

  clean_data <- allometry_data %>%
    dplyr::filter(!is.na(length_mm))

  return(clean_data)
}

##' The model fit function.
##'
##' @title Fit model
##'
##' @param x Data on which to perform the model fit
##'
##' @keywords internal
##' @export
fit_model <- function(x){
  lm(log10(biomass_mg) ~ log10(length_mm), data = x)
}

##' Run the linear models to create equations for calculating biomass.
##'
##' @title Run linear models
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @keywords internal
##' @import dplyr
##' @export
run_linear_models <- function(allometry_data){

  # Filter out species with only one measurement
  filtered_data <- allometry_data %>%
    dplyr::filter(!is.na(biomass_mg)) %>%
    dplyr::group_by(bwg_name, stage) %>%
    dplyr::mutate(n = n()) %>%
    ungroup() %>%
    dplyr::filter(n > 1)

  # Group the data and run model fits
  model_fits <- filtered_data %>%
    dplyr::group_by(bwg_name, stage, biomass_type) %>%
    tidyr::nest() %>%
    mutate(fit  = purrr::map(data, fit_model)) %>%
    mutate(tidy = purrr::map(fit, broom::tidy)) %>%
    mutate(glance = purrr::map(fit, broom::glance))

  return(model_fits)
}

##' Create the final equation bank by getting summary data.
##'
##' @title Obtain summary data
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
obtain_summary_data <- function(allometry_data){
  # Obtain the summary data, creating the final equation bank
  eqn_bank <- allometry_data %>%
    dplyr::mutate(r_squared   = purrr::map_dbl(glance, "r.squared")) %>%
    dplyr::mutate(sample_size = purrr::map_int(data, nrow)) %>%
    dplyr::select(-data, -glance) %>%
    tidyr::unnest(tidy, .drop = FALSE) %>%
    # tidyr::unnest(tidy) %>%
    dplyr::mutate(term = recode(term,
                                `(Intercept)` = "intercept",
                                `log10(length_mm)` = "slope")) %>%
    dplyr::select(-statistic, -p.value, -std.error) %>%
    tidyr::spread(key = term, value = estimate)

  # Retain the error on the estimates
  # dplyr::mutate(term = recode(term,
  #                           `(Intercept)` = "intercept",
  #                           `log10(length_mm)` = "slope")) %>%
  # dplyr::select(-statistic, -p.value) %>%
  # tidyr::gather(estimate:std.error, key = measurement, value = value) %>%
  # unite(name, c(term, measurement)) %>%
  # tidyr::spread(key = name, value = value)

  return(eqn_bank)
}
