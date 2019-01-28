#' PM4PY for R
#'
#' This package provides acces to the Python Process Mining library PM4PY in R
#' and provides conversion between bupaR and PM4PY data structures.
#'
#' To use this package, you need to have a Python environment (Conda or virtualenv)
#' installed and install the PM4PY package and its dependencies. You can use
#' the convenience function \link{install_pm4py} to let `reticulate` take care of
#' install the right version. See the documentation of this function for further
#' information.
#'
#' When loaded, the object `pm4py` provides the low-level interface to the main
#' PM4PY module. Use `$` to access sub modules of PM4PY as described in the
#' `reticulate` documentation:
#'
#' \code{vignette("calling_python", package = "reticulate")}
#'
#' For parts of PM4PY wrapper functions are provided to transparently convert
#' parameters and results to and from the corresponding bupaR S3 classes.
#'
#' @examples
#' \dontrun{
#' # Print the PM4PY version loaded
#' pm4py$`__version__`
#' }
#'
#' @import reticulate
#' @export
pm4py <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to pm4py
  pm4py <<- import("pm4py", delay_load = TRUE)
}

pm4py_tools <- function() {
  python_path <- system.file("python", package = "pm4py")
  pm4py_tools <- import_from_path("pm4pytools", path = python_path)
}

constant_xes_traceid_key <- function() {
  pm4py$objects$log$util$xes$DEFAULT_TRACEID_KEY
}

constant_xes_name_key <- function() {
  pm4py$objects$log$util$xes$DEFAULT_NAME_KEY
}

constant_xes_timestamp_key <- function() {
  pm4py$objects$log$util$xes$DEFAULT_TIMESTAMP_KEY
}

constant_xes_resource_key <- function() {
  pm4py$objects$log$util$xes$DEFAULT_RESOURCE_KEY
}

constant_xes_transition_key <- function() {
  pm4py$objects$log$util$xes$DEFAULT_RESOURCE_KEY
}
