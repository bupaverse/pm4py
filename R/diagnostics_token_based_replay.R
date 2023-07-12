# Diagnostics token-based replay ------------------------------------------
#' @title Apply __the token-based replay__ algorithm between a log and a process model
#'
#' @description Apply token-based replay for conformance checking analysis. The methods return the full token-based-replay diagnostics.
#'
#' @inheritParams discover_inductive
#' @inheritParams fitness_alignments
#'
#' @return Token-based-replay diagnostics.
#'
#' @export
diagnostics_token_based_replay <- function(log,
                                           marked_petrinet,
                                           convert = TRUE) {
  UseMethod("diagnostics_token_based_replay")
}

#' @export
diagnostics_token_based_replay.log <- function(log,
                                           marked_petrinet,
                                           convert = TRUE) {
  pm4py_conformance <- reticulate::import("pm4py.conformance", convert = convert)

  if(n_events(log) > n_activity_instances(log)) {
    cli::cli_warn("Activity instances with multiple events found in log: using only complete events.")


    if("eventlog" %in% class(log)) {
      log %>%
        filter(.data[[lifecycle_id(log)]] == "complete") -> log
    }
  }

  log %>%
    mutate(across(activity_id(log), as.character)) -> log
  # prepare arguments for pm4py module
  py_log <- r_to_py(log)
  py_pn <- as_py_value(marked_petrinet$petrinet)
  im <- as_pm4py_marking(marked_petrinet$initial_marking, py_pn)
  fm <- as_pm4py_marking(marked_petrinet$final_marking, py_pn)
  activity_key <- bupaR::activity_id(log)
  timestamp_key <- bupaR::timestamp(log)
  case_id_key <- bupaR::case_id(log)

  pm4py_conformance$conformance_diagnostics_token_based_replay(log = py_log, petri_net = py_pn,
                                                               initial_marking = im,
                                                               final_marking = fm,
                                                               activity_key = activity_key,
                                                               timestamp_key = timestamp_key,
                                                               case_id_key = case_id_key)
}

