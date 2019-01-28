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
