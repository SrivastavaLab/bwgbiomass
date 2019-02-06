##' Create a new fwdata release using the most up to date version
##' (\url{https://github.com/SrivastavaLab/cesabfunctionalwebsdata/})
##' @title Create a new fwdata release
##'
##' @param data_dir A local data directory in which to save the fwdata including
##'   the biomass data frame
##'
##' @param version Optional data version number
##'
##' @import fwdata
##' @importFrom dplyr %>%
##' @keywords internal
##' @export
fwdata_create <- function(data_dir = "_data", version){
  # I/O Variables
  filename <- paste(data_dir, "fwdata_v.", version, ".rds", sep = "")

  # Data acquisition
  fwdata <- fwdata::fw_data()

  biomass <- gen_biomass()

  all_data_list <- fwdata
  all_data_list$biomass <- biomass

  # Save file
  saveRDS(object = all_data_list, file = filename)

  return(invisible())
}
