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
#' @export
pm4py <- NULL
