#' @import reticulate
NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to pm4py
  pm4py <<- import("pm4py", delay_load = TRUE)
}

#' Returns pm4py version used
#'
#' @return package_version S3 class
#' @export
#'
#' @examples
#' \dontrun{
#' # Prints PM4PY version used
#' version()
#' }
version <- function() {
  ver <- pm4py$`__version__`
  ver <- regmatches(ver, regexec("^([0-9\\.]+).*$", ver))[[1]][[2]]
  package_version(ver)
}
