#' Returns PM4Py version used
#'
#' @return package_version S3 class
#' @export
#'
#' @examples
#' if (pm4py_available()) {
#'   print(version())
#' }
version <- function() {
  ver <- pm4py$`__version__`
  ver <- regmatches(ver, regexec("^([0-9\\.]+).*$", ver))[[1]][[2]]
  package_version(ver)
}

#' Is the PM4Py module available
#'
#' @return `TRUE` is PM4Py is installed
#' @export
#'
#' @examples
#' if (pm4py_available()) {
#'   print(version())
#' }
#'
pm4py_available <- function() {
  py_module_available("pm4py")
}
