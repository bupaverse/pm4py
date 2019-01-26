#' Install PM4PY library
#'
#' Installs the `pm4py` package and its dependencies using `pip` since no
#' Conda package is available. Further information on the parameters can
#' be found in the `reticulate` package documentation:
#' https://rstudio.github.io/reticulate/
#'
#' Additional requirements (a C++ compiler and GraphViz) of PM4PY might
#' need to be installed to leverage all functionality;
#' http://pm4py.pads.rwth-aachen.de/installation/
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment.
#'  Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda using the PATH and other conventional install locations).
#' @param ... Additional arguments passed to py_install().
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_pm4py()
#' }
#'
install_pm4py <- function(method = "auto", conda = "auto", ...) {
  reticulate::py_install("pm4py==1.0.19", method = method, conda = conda, pip = TRUE, ...)
}
