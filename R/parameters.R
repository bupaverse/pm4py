#' PM4PY parameter keys
#'
#' Convenience methods to use PM4PY parameter keys.
#'
#' @param value The value to add to the list.
#'
#' @return a list with the parameter key/value pair
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

pm4py_parameters <- list(
  activity_key = "pm4py:param:activity_key",
  attribute_key = "pm4py:param:attribute_key",
  timestamp_key = "pm4py:param:timestamp_key",
  caseid_key = "case_id_glue",
  resource_key = "pm4py:param:resource_key"
)
