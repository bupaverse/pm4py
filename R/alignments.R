#' Alignment between an Event Log and a Petri net
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param petrinet A bupaR or PM4PY Petri net.
#' @param initial_marking A R vector with the place identifiers of the initial marking or a PM4PY marking.
#' By default the initial marking of the bupaR Petri net will be used if available.
#' @param final_marking A R vector with the place identifiers of the final marking or a PM4PY marking.
#' @param parameters PM4PY alignment parameter.
#' By default the `activity_key` from the bupaR event log is specified using \link{param_activity_key}.
#' @param variant The alignment variant to be used.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent. If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A `data.frame` describing the alignment in terms of log and model moves.
#' @export
#'
#' @examples
#' \dontrun{
#' net <- discovery_inductive(patients)
#'
#' a <- conformance_alignment(patients,
#'                            net$petrinet,
#'                            net$initial_marking,
#'                            net$final_marking)
#' }
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
conformance_alignment <- function(eventlog,
                                  petrinet,
                                  initial_marking,
                                  final_marking,
                                  parameters = param_activity_key(bupaR::activity_id(eventlog)),
                                  variant = variant_state_equation_a_star(),
                                  convert = TRUE) {

  pm4py_alignments <- import("pm4py.algo.conformance.alignments.factory", convert = convert)
  if (is.null(initial_marking) && inherits(petrinet, "petrinet")) {
    initial_marking <- petrinet$marking
  }

  py_pn <- as_py_value(petrinet)
  py_log <- as_py_value(eventlog)

  alignment <- pm4py_alignments$apply(obj = py_log,
                                      petri_net = py_pn,
                                      initial_marking = as_pm4py_marking(initial_marking, py_pn),
                                      final_marking = as_pm4py_marking(final_marking, py_pn),
                                      parameters = parameters,
                                      version = variant)

  if (convert) {

    case_ids <- eventlog %>% as.data.frame() %>%
      distinct(!!as.symbol(bupaR::case_id(eventlog))) %>%
      pull()

    purrr::map2_dfr(alignment, case_ids, function(trace, case_id) {

      align_mat <- t(sapply(trace$alignment, c)) # convert nested lists to matrix
      align_mat[vapply(align_mat, is.null, TRUE)] <- NA_character_
      align_lst <- apply(align_mat, 2, unlist) # remove wrapper lists from elements

      trace_df <- data.frame(align_lst, stringsAsFactors = FALSE)
      names(trace_df) <- c("logmove", "modelmove")

      cbind(case_id, trace_df, trace[-1],  # add meta information by duplicating it since we don't have a trace object
            stringsAsFactors = FALSE)
    })
  } else {
    alignment
  }
}

#'A* search using the state equation as heuristic
#'
#' @export
variant_state_equation_a_star <- function() {
  pm4py$algo$conformance$alignments$factory$VERSION_STATE_EQUATION_A_STAR
}


