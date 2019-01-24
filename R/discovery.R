#' Petri net discovery algorithms
#'
#' PM4PY discovery algorithms that discover a Petri net and its initial and final marking. Currently the Inductive Miner and the Alpha Miner are implemented.
#'
#' @param eventlog A bupaR event log.
#' @param parameters A named list of PM4PY parameters, by default the `activity_key` from the bupaR event log is used. Use \link{param_activity_key} to specifiy a different key.
#' @param variant The variant of the discovery algorithm to be used. For Inductive Miner currently only `variant_inductive_only_dfg` is supported.
#' @param convert TRUE to automatically convert Python objects to their R equivalent. If you pass FALSE you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A named list with elements `petrinet`, `initial_marking`, and `final_marking` or the original Python object.
#'
#' @examples
#' \dontrun{
#' data(patients)
#' net <- discovery_inductive(patients)
#'
#' # Show details of the obtained bupaR Petri net
#' print(net$petrinet)
#'
#' # initial marking is a character vector
#' print(net$initial_marking)
#'
#' # final marking is a character vector
#' print(net$final_marking)
#'
#' # Petri net can be used with other bupaR functions
#' petrinetR::render_PN(net$petrinet)
#'
#' # Keep an unconverted PM4PY Petri net for use in other PM4PY functions
#' py_net <- discovery_inductive(patients, convert = FALSE)
#' }
#'
#' @name discovery
NULL

#' @rdname discovery
#' @import reticulate
#' @export
discovery_inductive <- function(eventlog,
                                parameters = param_activity_key(bupaR::activity_id(eventlog)),
                                variant = variant_inductive_only_dfg(),
                                convert = TRUE) {
  pm4py_inductive <- import("pm4py.algo.discovery.inductive.factory", convert = convert)
  model <- pm4py_inductive$apply(as_py_value(eventlog),
                                 parameters = parameters,
                                 variant = variant)
  prepare_pn_with_markings(model, convert)
}

#' @rdname discovery
#' @export
variant_inductive_only_dfg <- function() {
  pm4py$algo$discovery$inductive$factory$INDUCTIVE_ONLY_DFG
}

#' @rdname discovery
#' @import reticulate
#' @export
discovery_alpha <- function(eventlog,
                                parameters = param_activity_key(bupaR::activity_id(eventlog)),
                                variant = variant_alpha_classic(),
                                convert = TRUE) {
  pm4py_alpha <- import("pm4py.algo.discovery.alpha.factory", convert = convert)
  model <- pm4py_alpha$apply(as_py_value(eventlog),
                             parameters = parameters,
                             version = variant)
  prepare_pn_with_markings(model, convert)
}

#' @rdname discovery
#' @export
variant_alpha_classic <- function() {
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_CLASSIC
}
