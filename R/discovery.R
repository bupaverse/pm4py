#' Petri net discovery algorithms
#'
#' PM4PY discovery algorithms that discover a Petri net and its initial and final marking. Currently the Inductive Miner and the Alpha Miner are implemented.
#'
#' @param eventlog A bupaR event log or an R data frame.
#' @param parameters A named list of PM4PY parameters (see \link{parameters}) as required by the discovery method.
#'  By default, if the `eventlog` is a bupaR event log, the `activity_key`, `timestamp_key`, and `caseid_key` are automatically determined.
#' @param variant The variant of the discovery algorithm to be used.
#'  For Inductive Miner currently only `variant_inductive_imdfb` is supported.
#' @param convert TRUE to automatically convert Python objects to their R equivalent.
#'  If you pass FALSE you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A named list with elements `petrinet`, `initial_marking`, and `final_marking` or the original Python object.
#'
#' @examples
#' if (pm4py_available()) {
#'   library(eventdataR)
#'   data(patients)
#'
#'   # As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
#'   patients_completes <- patients[patients$registration_type == "complete", ]
#'
#'   net <- discovery_inductive(patients_completes)
#'
#'   # Show details of the obtained bupaR Petri net
#'   print(net$petrinet)
#'
#'   # initial marking is a character vector
#'   print(net$initial_marking)
#'
#'   # final marking is a character vector
#'   print(net$final_marking)
#'
#'   # Petri net can be used with other bupaR functions
#'   petrinetR::render_PN(net$petrinet)
#'
#'   # Keep an unconverted PM4PY Petri net for use in other PM4PY functions
#'   py_net <- discovery_inductive(patients_completes, convert = FALSE)
#' }
#'
#' @name discovery
NULL

#' @rdname discovery
#' @export
discovery_inductive <- function(eventlog,
                                convert = TRUE) {
  pm4py_inductive <- reticulate::import("pm4py.discovery", convert = convert)

  model <- pm4py_inductive$discover_petri_net_inductive(r_to_py(eventlog),
                                                        activity_key = "handling",
                                                        timestamp_key = "time",
                                                        case_id_key = "patient")

  # model[[1]]$transitions %>% names -> nested_list_elements
  # model[[1]]$transitions[nested_list_elements != "label"] %>% unlist() -> id
  # model[[1]]$transitions[nested_list_elements == "label"] %>% unlist() -> label



  create_marked_PN(model)
}

#' @rdname discovery
#' @export
variant_inductive_imdfb <- function() {
  pm4py$algo$discovery$inductive$factory$IMDFB
}


#' @rdname discovery
#' @export
variant_inductive_only_dfg <- function() {
  .Deprecated("variant_inductive_imdfb")
  pm4py$algo$discovery$inductive$factory$IMDFB
}

#' @rdname discovery
#' @export
discovery_alpha <- function(eventlog,
                                parameters = default_parameters(eventlog),
                                variant = variant_alpha_classic(),
                                convert = TRUE) {
  pm4py_alpha <- reticulate::import("pm4py.algo.discovery.alpha.factory", convert = convert)
  model <- pm4py_alpha$apply(as_py_value(eventlog),
                             parameters = parameters,
                             variant = variant)
  prepare_pn_with_markings(model, convert)
}

#' @rdname discovery
#' @export
variant_alpha_classic <- function() {
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_CLASSIC
}

#' @rdname discovery
#' @export
variant_alpha_plus <- function() {
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_PLUS
}
