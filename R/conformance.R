#' Conformance between an Event Log and a Petri net
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param petrinet A bupaR or PM4PY Petri net.
#' @param initial_marking A R vector with the place identifiers of the initial marking or a PM4PY marking.
#' By default the initial marking of the bupaR Petri net will be used if available.
#' @param final_marking A R vector with the place identifiers of the final marking or a PM4PY marking.
#' @param parameters PM4PY conformance parameter.
#' By default the `activity_key` from the bupaR event log is specified using \link{param_activity_key}.
#' @param variant The conformance variant to be used.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent.
#'  If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A data frame describing the conformance result.
#'  In case of `conformance_alignment` a data frame of log and model moves.
#'
#' @name conformance
#'
#' @examples
#' if (pm4py_available()) {
#'   library(eventdataR)
#'   data(patients)
#'
#'   # As Inductive Miner of PM4Py is not life-cycle aware, keep only `complete` events:
#'   patients_completes <- patients[patients$registration_type == "complete", ]
#'
#'   # Discover a Petri net
#'   net <- discovery_inductive(patients_completes)
#'
#'   # Align event log and Petri net
#'   a <- conformance_alignment(patients_completes,
#'                              net$petrinet,
#'                              net$initial_marking,
#'                              net$final_marking)
#'
#'   # Alignment is returned as data frame
#'   head(a)
#' }
#'
NULL


#' @rdname conformance
#' @export
#' @import reticulate
conformance_alignment <- function(eventlog,
                                  petrinet,
                                  initial_marking,
                                  final_marking,
                                  parameters = default_parameters(eventlog),
                                  variant = variant_state_equation_a_star(),
                                  convert = TRUE) {

  pm4py_alignments <- import("pm4py.algo.conformance.alignments.factory", convert = convert)
  if (is.null(initial_marking) && inherits(petrinet, "petrinet")) {
    initial_marking <- petrinet$marking
  }

  py_pn <- as_py_value(petrinet)
  py_log <- as_py_value(eventlog)

  if (variant == variant_state_equation_a_star()) {
    param_syncaware <- list(TRUE)
    names(param_syncaware) <- pm4py$algo$conformance$alignments$versions$
        state_equation_a_star$PARAM_ALIGNMENT_RESULT_IS_SYNC_PROD_AWARE
    parameters <- c(parameters, param_syncaware)
  }

  alignment <- pm4py_alignments$apply(obj = py_log,
                                      petri_net = py_pn,
                                      initial_marking = as_pm4py_marking(initial_marking, py_pn),
                                      final_marking = as_pm4py_marking(final_marking, py_pn),
                                      parameters = parameters,
                                      version = variant)

  if (convert) {

    case_ids <- pm4py_tools()$log$get_trace_ids(py_log, parameters)

    df_alignment <- purrr::map2_dfr(alignment, case_ids, function(trace, case_id) {

      align_mat <- t(sapply(trace$alignment, function(x) {
          c(x[[1]], x[[2]])
      })) # convert nested lists to matrix
      align_mat[vapply(align_mat, is.null, TRUE)] <- NA_character_
      align_lst <- apply(align_mat, 2, unlist) # remove wrapper lists from elements

      trace_df <- data.frame(align_lst, stringsAsFactors = FALSE)
      names(trace_df) <- c("log_id", "model_id", "log_label", "model_label")

      # add meta information by duplicating it since we don't have a trace object
      cbind(case_id,
            trace_df,
            trace[-1], stringsAsFactors = FALSE)

    })

    class(df_alignment) <- c("alignment", class(df_alignment))

    df_alignment

  } else {
    alignment
  }
}

#' @rdname conformance
#' @export
variant_state_equation_a_star <- function() {
  pm4py$algo$conformance$alignments$factory$VERSION_STATE_EQUATION_A_STAR
}
