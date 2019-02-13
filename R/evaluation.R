#' Calculates evaluation measures for a Petri nets and an Event Log
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param petrinet A bupaR or PM4PY Petri net.
#' @param initial_marking A R vector with the place identifiers of the initial marking or a PM4PY marking.
#' By default the initial marking of the bupaR Petri net will be used if available.
#' @param final_marking A R vector with the place identifiers of the final marking or a PM4PY marking.
#' @param parameters PM4PY alignment parameter.
#' By default the `activity_key` from the bupaR event log is specified using \link{param_activity_key}.
#' @param variant The evaluation variant to be used.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent. If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A `list` with all available evaluation measures.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#'
#' # As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
#' patients_completes <- patients[patients$registration_type == "complete", ]
#'
#' # Discover a Petri net
#' net <- discovery_inductive(patients_completes)
#'
#' # Calculate evaluation measures for event log and Petri net
#' evaluation_all(patients_completes,
#'                net$petrinet,
#'                net$initial_marking,
#'                net$final_marking)
#'
#' }
#'
#' @name evaluation
NULL

#' @rdname evaluation
#' @import reticulate
#' @export
evaluation_all <- function(eventlog,
                           petrinet,
                           initial_marking,
                           final_marking,
                           parameters = default_parameters(eventlog),
                           convert = TRUE) {
  pm4py_evaluation <- import("pm4py.evaluation.factory", convert = convert)

  py_pn <- as_py_value(petrinet)
  py_log <- as_py_value(eventlog)

  m <- pm4py_evaluation$apply(log = py_log,
                              net = py_pn,
                              initial_marking = as_pm4py_marking(initial_marking, py_pn),
                              final_marking = as_pm4py_marking(final_marking, py_pn),
                              parameters = parameters)
  m
}

#' @rdname evaluation
#' @import reticulate
#' @export
evaluation_precision <- function(eventlog,
                               petrinet,
                               initial_marking,
                               final_marking,
                               parameters = default_parameters(eventlog),
                               variant = variant_precision_etconformance(),
                               convert = TRUE) {
  pm4py_evaluation <- import("pm4py.evaluation.precision.factory", convert = convert)

  py_pn <- as_py_value(petrinet)
  py_log <- as_py_value(eventlog)

  m <- pm4py_evaluation$apply(log = py_log,
                              net = py_pn,
                              marking = as_pm4py_marking(initial_marking, py_pn), # TODO inconsistent naming upstream
                              final_marking = as_pm4py_marking(final_marking, py_pn),
                              parameters = parameters,
                              variant = variant)
  m
}

#' @rdname evaluation
#' @export
variant_precision_etconformance <- function() {
  pm4py$evaluation$precision$factory$ETCONFORMANCE_TOKEN
}

#' @rdname evaluation
#' @import reticulate
#' @export
evaluation_fitness <- function(eventlog,
                               petrinet,
                               initial_marking,
                               final_marking,
                               parameters = default_parameters(eventlog),
                               variant = variant_fitness_token_based(),
                               convert = TRUE) {
  pm4py_evaluation <- import("pm4py.evaluation.replay_fitness.factory", convert = convert)

  py_pn <- as_py_value(petrinet)
  py_log <- as_py_value(eventlog)

  m <- pm4py_evaluation$apply(log = py_log,
                              petri_net = py_pn, # TODO inconsistent naming upstream
                              initial_marking = as_pm4py_marking(initial_marking, py_pn),
                              final_marking = as_pm4py_marking(final_marking, py_pn),
                              parameters = parameters,
                              variant = variant)
  m
}

#' @rdname evaluation
#' @export
variant_fitness_token_based <- function() {
  pm4py$evaluation$replay_fitness$factory$TOKEN_BASED
}

#' @rdname evaluation
#' @export
variant_fitness_alignment_based <- function() {
  pm4py$evaluation$replay_fitness$factory$ALIGNMENT_BASED
}
