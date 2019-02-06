##' Filter by data type.
##'
##' @title Filter by data type
##'
##' @param allometry_data The allometry data set loaded from:
##'   \url{https://github.com/SrivastavaLab/allometrydata/}
##'
##' @param measurement logical indicating whether the measurements should be filtered into
##'   the final data set (type = field_measurement). Otherwise, the category
##'   definitions will be filtered into the final data set
##'   (type = category_definition). Defaults to \emph{measurement = TRUE}
##'
##' @return the filtered data set
##'
##' @keywords internal
##' @importFrom dplyr %>%
##' @export
filter_type <- function(allometry_data, measurement = TRUE){
  if(measurement){
    filtered_data <- allometry_data %>%
      dplyr::filter(type == "field_measurement")
  } else {
    filtered_data <- allometry_data %>%
      dplyr::filter(type == "category_definition")
  }

  return(filtered_data)
}
