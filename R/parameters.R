#' PM4Py parameter keys
#'
#' Convenience methods to use as PM4Py parameter keys.
#'
#' @param value The value to add to the list.
#' @return a list with the parameter key/value pair
#' @examples
#' param_activity_key("activity")
#'
#' library(eventdataR)
#' data(patients)
#' default_parameters(patients)
#'
#' @name parameters
NULL

#' @rdname parameters
#' @export
param_activity_key <- function(value) {
  l <- list(value)
  names(l) <- pm4py_parameters$activity_key
  l
}

#' @rdname parameters
#' @export
param_attribute_key <- function(value) {
  l <- list(value)
  names(l) <- pm4py_parameters$attribute_key
  l
}

#' @rdname parameters
#' @export
param_timestamp_key <- function(value) {
  l <- list(value)
  names(l) <- pm4py_parameters$timestamp_key
  l
}

#' @rdname parameters
#' @export
param_caseid_key <- function(value) {
  l <- list(value)
  names(l) <- pm4py_parameters$caseid_key
  l
}

#' @rdname parameters
#' @export
param_resource_key <- function(value) {
  l <- list(value)
  names(l) <- pm4py_parameters$resource_key
  l
}

#' @param eventlog A bupaR or PM4PY event log.
#'
#' @rdname parameters
#' @export
default_parameters <- function(eventlog) {
  if ("eventlog" %in% class(eventlog)) {
    c(param_activity_key(bupaR::activity_id(eventlog)),
      param_timestamp_key(bupaR::timestamp(eventlog)),
      param_caseid_key(bupaR::case_id(eventlog)))
  } else {
    list()
  }
}

pm4py_parameters <- list(
  activity_key = "pm4py:param:activity_key",
  attribute_key = "pm4py:param:attribute_key",
  timestamp_key = "pm4py:param:timestamp_key",
  caseid_key = "case_id_glue",
  resource_key = "pm4py:param:resource_key"
)
