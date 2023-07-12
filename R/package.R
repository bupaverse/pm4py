#' Returns PM4Py version used
#'
#' @return package_version S3 class
#' @export
#'
pm4py_version <- function() {
  if (pm4py_available()) {
    ver <- pm4py$`__version__`
    ver <- regmatches(ver, regexec("^([0-9\\.]+).*$", ver))[[1]][[2]]
    package_version(ver)
  } else {
    stop("No PM4Py installation found, please install it by using `pm4py::install_pm4py()`!")
  }
}

#' This function is deprecated, please use \code{\link{pm4py_version}}.
#'
#' @return package_version S3 class
#' @export
#'
version <- function() {
  .Deprecated("pm4py_version")
  pm4py_version()
}


#' Is the PM4Py module available
#'
#' @return `TRUE` is PM4Py is installed
#' @export
#'
#'
pm4py_available <- function() {
  py_module_available("pm4py")
}
