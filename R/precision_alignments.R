# PRECISION ALIGNMENTS ----------------------------------------------------
#' @title Apply the __precision__ alignments algorithm between a log and a process model
#'
#' @description Calculates the precision of the model w.r.t. the event log using alignments.
#'
#' @inheritParams discover_inductive
#' @inheritParams fitness_alignments
#' @return precision metric.
#' @seealso \url{https://pm4py.fit.fraunhofer.de/static/assets/api/2.3.0/generated/pm4py.conformance.precision_alignments.html}
#'
#' @export
precision_alignments <- function(log,
                                 marked_petrinet,
                                 multi_processing = FALSE,
                                 convert = TRUE) {
  UseMethod("precision_alignments")
}
#' @export
precision_alignments.log <- function(log,
                                          marked_petrinet,
                                          multi_processing = FALSE,
                                          # petrinet,
                                          # initial_marking,
                                          # final_marking,
                                          # activity_key,
                                          # timestamp_key,
                                          # case_id_key,
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
  multi_processing <- r_to_py(multi_processing)


  pm4py_conformance$precision_alignments(log = py_log, petri_net = py_pn,
                                         initial_marking = im,
                                         final_marking = fm,
                                         activity_key = activity_key,
                                         timestamp_key = timestamp_key,
                                         case_id_key = case_id_key,
                                         multi_processing = multi_processing)
}
