#' Install PM4PY library
#'
#' Installs the `pm4py` package and its dependencies using `pip` since no
#' Conda package is available. Further information on the parameters can
#' be found in the `reticulate` package documentation:
#' https://rstudio.github.io/reticulate/
#' In some cases (multiple Python versions installed) it might be useful to specify the exact path to the `conda` binary.
#'
#' Additional requirements, for example, a C++ compiler and GraphViz might
#' need to be installed to leverage all functionality. Please refer to the PM4Py documentation for details:
#' https://pm4py.fit.fraunhofer.de/install
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment.
#'  Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda using the PATH and other conventional install locations).
#' @param version Optional parameter overriding the PM4Py version that will be installed. Note that the R pm4py package was only tested against the default PM4Py version that will be installed.
#' @param ... Additional arguments passed to py_install().
#'
#' @export
#'
#'
install_pm4py <- function(method = "auto", conda = "auto", version = NULL, ...) {

  if (!is.null(version)) {
    version <- paste0("pm4py==", version)
  } else {
    version <- PM4PY_PIP
  }

  tryCatch({
    if (PM4PY_DEVELOPMENT) {
      reticulate::py_install(paste0("-r", PM4PY_DEVELOPMENT_DEPENDENCIES), method = method, conda = conda, pip = TRUE, ...)
    }
    reticulate::py_install(version, method = method, conda = conda, pip = TRUE, ...)
  },
    error = function(e) {
      # workaround for virtualenv not supporting `pip` parameter
      warning("Could not find `conda` environment, falling back to virtualenv ... ")
      reticulate::py_install(version, method = method, conda = conda, ...)
    }
  )
}
