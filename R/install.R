#' Install pm4py library
#'
#' Installs the pm4py into a virtualenv or conda env.
#'
#' @param method Installation method. By default, "auto" automatically finds a method that will work in the local environment. Change the default to force a specific installation method. Note that the "virtualenv" method is not available on Windows.
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
  reticulate::py_install("pm4py", method = method, conda = conda, pip = TRUE, ...)
}
