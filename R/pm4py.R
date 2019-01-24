#' PM4PY for R
#'
#' This package provides acces to the Python Process Mining library PM4PY in R
#' and provides conversion between bupaR and PM4PY data structures.
#'
#' The object `pm4py` provides the interface to the main PM4PY module .
#' Use `$` to access sub modules of PM4PY as described in the `reticulate`
#' documentation:
#' \code{vignette("calling_python", package = "reticulate")}
#'
#' @examples
#' \dontrun{
#' # Print the PM4PY version loaded
#' pm4py$`__version__`
#' }
#'
#' @export
pm4py <- NULL
