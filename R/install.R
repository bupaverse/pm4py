#' Install PM4PY library
#'
#' Installs the `pm4py` package and its dependencies using `pip` since no
#' Conda package is available. Further information on the parameters can
#' be found in the `reticulate` package documentation:
#' https://rstudio.github.io/reticulate/
#' In some cases (multiple Python versions installed) it might be useful to specify the exact path to the `conda` binary.
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
#' \donttest{
#'   pm4py::install_pm4py()
#'
#'   # Specify path to conda
#'   pm4py::install_pm4py(method = "conda", conda = "/home/user/miniconda3/bin/conda")
#' }
#'
install_pm4py <- function(method = "auto", conda = "auto", ...) {

  tryCatch({
    if (PM4PY_DEVELOPMENT) {
      reticulate::py_install(paste0("-r", PM4PY_DEPENDENCIES), method = method, conda = conda, pip = TRUE, ...)
    }
    reticulate::py_install(PM4PY_VERSION, method = method, conda = conda, pip = TRUE, ...)
    # workaround for issue with PM4Py dependencies
    reticulate::py_install("ortools", method = method, conda = conda, pip = TRUE, ...)
  },
    error = function(e) {
      # workaround for virtualenv not supporting `pip` parameter
      warning("Could not find `conda` environment, falling back to virtualenv ... ")
      reticulate::py_install(PM4PY_VERSION, method = method, conda = conda, ...)
      # workaround for issue with PM4Py dependencies
      reticulate::py_install("ortools", method = method, conda = conda, ...)
    }
  )
}
