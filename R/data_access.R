##' Download the BWG biomass data from SrivastavaLab/bwgbiomass
##'  (\url{https://github.com/SrivastavaLab/bwgbiomass/})
##' @title Download BWG biomass data set
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{biomass_del}, specifying \code{version = NULL} will
##'   delete \emph{all} data sets.
##'
##' @param path Path to store the data at.  If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{biomass_del(NULL)} (or \code{biomass_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
biomass <- function(version = NULL, path = NULL) {
  datastorr::github_release_get(biomass_info(path), version)
}

##' @export
##' @rdname biomass
##'
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local = FALSE} is a superset of
##'   \code{local = TRUE}.  For \code{biomass_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
##'
biomass_versions <- function(local = TRUE, path = NULL) {
  datastorr::github_release_versions(biomass_info(path), local)
}

##' @export
##' @rdname biomass
biomass_version_current <- function(local = TRUE, path = NULL) {
  datastorr::github_release_version_current(biomass_info(path), local)
}

##' @export
##' @rdname biomass
biomass_del <- function(version, path = NULL) {
  datastorr::github_release_del(biomass_info(path), version)
}

## Core data:
biomass_info <- function(path) {
  datastorr::github_release_info("SrivastavaLab/bwgbiomass",
                                 filename = NULL,
                                 read = readRDS,
                                 path = path)
}

##' Download the allometry data set from SrivastavaLab/allometrydata
##'  (\url{https://github.com/SrivastavaLab/allometrydata/})
##' @title Download allometry data set
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{allometry_del}, specifying \code{version = NULL} will
##'   delete \emph{all} data sets.
##'
##' @param path Path to store the data at.  If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{allometry_del(NULL)} (or \code{allometry_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
allometry <- function(version = NULL, path = NULL) {
  datastorr::github_release_get(allometry_info(path), version)
}

##' @export
##' @rdname allometry
##'
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local = FALSE} is a superset of
##'   \code{local = TRUE}.  For \code{allometry_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
##'
allometry_versions <- function(local = TRUE, path = NULL) {
  datastorr::github_release_versions(allometry_info(path), local)
}

##' @export
##' @rdname allometry
allometry_version_current <- function(local = TRUE, path = NULL) {
  datastorr::github_release_version_current(allometry_info(path), local)
}

##' @export
##' @rdname allometry
allometry_del <- function(version, path = NULL) {
  datastorr::github_release_del(allometry_info(path), version)
}

## Core data:
allometry_info <- function(path) {
  datastorr::github_release_info("SrivastavaLab/allometrydata",
                                 filename = NULL,
                                 read = readRDS,
                                 path = path)
}