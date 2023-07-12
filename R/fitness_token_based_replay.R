#' Measure Token-Based Replay Fitness
#'
#' @inheritParams discover_inductive
#' @inheritParams fitness_alignments
#'
#' @return List with fitness measures.
#'
#' @examples
#' \dontrun{
#' library(pm4py)
#' library(eventdataR)
#'
#' model <- discover_alpha(patients)
#' fitness_token_based_replay(patients, model)
#'
#' }
#' @export
#'

fitness_token_based_replay <- function(log,
                               marked_petrinet,
                               convert = TRUE) {
  UseMethod("fitness_token_based_replay")
}

#' @export
fitness_token_based_replay.eventlog <- function(log,
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


  pm4py_conformance$fitness_token_based_replay(log = py_log, petri_net = py_pn,
                                       initial_marking = im,
                                       final_marking = fm,
                                       activity_key = activity_key,
                                       timestamp_key = timestamp_key,
                                       case_id_key = case_id_key)
}
