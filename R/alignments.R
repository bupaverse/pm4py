

# DIAGNOSTICS ALIGNMENTS --------------------------------------------------
#' @title Apply the alignments algorithm between a log and a process model
#'
#' @description Alignment-based replay aims to find one of the best alignment between the trace and the model.
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param marked_petrinet_list [`list`]: An object of type `list` returned by [`discovery_inductive()`] and containing 3 elements:
#' * petri net (list of places, transitions, flows)
#' * initial marking: place identifier of the initial marking.
#' * final marking: place identifier of the final marking.
#' @param multi_processing [lgl] (default [`FALSE`]): Disables if `FALSE`, enables if `TRUE` multiprocessing in inductive miner.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent.
#'  If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return alignment diagnostics.
#'
#' @examples
#' # As Inductive Miner of PM4Py is not life-cycle aware, keep only `complete` events:
#'   patients_completes <- patients[patients$registration_type == "complete", ]
#'
#' # Discover a Petri net
#'   net <- discovery_inductive(patients_completes)
#'
#' # Align event log and Petri net
#' a <- conformance_alignment(patients_completes,
#'                              net$petrinet,
#'                              net$initial_marking,
#'                              net$final_marking)
#'
#' # Alignment is returned as data frame
#' head(a)
#'
#'
#' @seealso \url{https://pm4py.fit.fraunhofer.de/static/assets/api/2.3.0/pm4py.html#pm4py.conformance.conformance_diagnostics_alignments}
#' @import reticulate
#'
#' @export
conformance_diagnostics_alignments <- function(eventlog,
                                               marked_petrinet_list,
                                               marked_petrinet_list,
                                               multi_processing = FALSE,
                                               convert = TRUE, ...) {
  UseMethod("conformance_diagnostics_alignments")
}
#' @export
conformance_diagnostics_alignments.eventlog <- function(eventlog,
                                                        marked_petrinet_list,
                                                        multi_processing = FALSE,
                                                        # petrinet,
                                                        # initial_marking,
                                                        # final_marking,
                                                        # activity_key,
                                                        # timestamp_key,
                                                        # case_id_key,
                                                        convert = TRUE, ...) {

  pm4py_conformance <- reticulate::import("pm4py.conformance", convert = convert)


  # prepare arguments for pm4py module
  py_log <- r_to_py(eventlog)
  py_pn <- as_py_value(marked_petrinet_list$petrinet)
  im <- as_pm4py_marking(marked_petrinet_list$initial_marking, py_pn)
  fm <- as_pm4py_marking(marked_petrinet_list$final_marking, py_pn)
  activity_key <- bupaR::activity_id(eventlog)
  timestamp_key <- bupaR::timestamp(eventlog)
  case_id_key <- bupaR::case_id(eventlog)
  multi_processing <- r_to_py(multi_processing)

  # specs_list <- list(...)
  # specs <- list()
  # for (i in specs_list) {
  #   if (class(i) == "petrinet") {
  #     i <- as_py_value(i)
  #   }
  #   else {
  #     i <- as_pm4py_marking(i, py_pn)
  #   }
  #   specs <- specs %>% append(i)
  # }
  #
  # names(specs) <- c("py_pn", "im", "fm")


  pm4py_conformance$conformance_diagnostics_alignments(py_log, py_pn, im, fm, # specs
                                                       activity_key = activity_key,
                                                       timestamp_key = timestamp_key,
                                                       case_id_key = case_id_key,
                                                       multi_processing = multi_processing)
                                                       # py_pn, im, fm)

#   if (convert) {
#
#     case_ids <- pm4py_tools()$log$get_trace_ids(py_log, parameters)
#
#     df_alignment <- purrr::map2_dfr(alignment, case_ids, function(trace, case_id) {
#
#       align_mat <- t(sapply(trace$alignment, function(x) {
#         c(x[[1]], x[[2]])
#       })) # convert nested lists to matrix
#       align_mat[vapply(align_mat, is.null, TRUE)] <- NA_character_
#       align_lst <- apply(align_mat, 2, unlist) # remove wrapper lists from elements
#
#       trace_df <- data.frame(align_lst, stringsAsFactors = FALSE)
#       names(trace_df) <- c("log_id", "model_id", "log_label", "model_label")
#
#       # add meta information by duplicating it since we don't have a trace object
#       cbind(case_id,
#             trace_df,
#             trace[-1], stringsAsFactors = FALSE)
#
#     })
#
#     class(df_alignment) <- c("alignment", class(df_alignment))
#
#     df_alignment
#
#   } else {
#     alignment
#   }
# }


}


# FITNESS ALIGNMENTS ------------------------------------------------------
#' @title Apply the __fitness__ alignments algorithm between a log and a process model
#'
#' @description The calculation of the replay fitness aim to calculate how much of the behavior in the log is admitted by the process model.
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param marked_petrinet_list [`list`]: An object of type `list` returned by [`discovery_inductive()`] and containing 3 elements:
#' * petri net (list of places, transitions, flows)
#' * initial marking: place identifier of the initial marking.
#' * final marking: place identifier of the final marking.
#' @param multi_processing [lgl] (default [`FALSE`]): Disables if `FALSE`, enables if `TRUE` multiprocessing in inductive miner.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent.
#'  If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return fitness alignments.
#' @seealso \url{https://pm4py.fit.fraunhofer.de/static/assets/api/2.3.0/generated/pm4py.conformance.fitness_alignments.html}
#'
#' @export
fitness_alignments <- function(eventlog,
                               marked_petrinet_list,
                               multi_processing = FALSE,
                               convert = TRUE) {
  UseMethod("fitness_alignments")
}

#' @export
fitness_alignments.eventlog <- function(eventlog,
                                        marked_petrinet_list,
                                        multi_processing = FALSE,
                                        # petrinet,
                                        # initial_marking,
                                        # final_marking,
                                        # activity_key,
                                        # timestamp_key,
                                        # case_id_key,
                                        # activity_key,
                                        # timestamp_key,
                                        # case_id_key,
                                        convert = TRUE) {
  pm4py_conformance <- reticulate::import("pm4py.conformance", convert = convert)

  # prepare arguments for pm4py module
  py_log <- r_to_py(eventlog)
  py_pn <- as_py_value(marked_petrinet_list$petrinet)
  im <- as_pm4py_marking(marked_petrinet_list$initial_marking, py_pn)
  fm <- as_pm4py_marking(marked_petrinet_list$final_marking, py_pn)
  activity_key <- bupaR::activity_id(eventlog)
  timestamp_key <- bupaR::timestamp(eventlog)
  case_id_key <- bupaR::case_id(eventlog)
  multi_processing <- r_to_py(multi_processing)

  pm4py_conformance$fitness_alignments(log = py_log, petri_net = py_pn,
                                       initial_marking = im,
                                       final_marking = fm,
                                       activity_key = activity_key,
                                       timestamp_key = timestamp_key,
                                       case_id_key = case_id_key,
                                       multi_processing = multi_processing)
}


# PRECISION ALIGNMENTS ----------------------------------------------------
#' @title Apply the __precision__ alignments algorithm between a log and a process model
#'
#' @description Calculates the precision of the model w.r.t. the event log using alignments.
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param marked_petrinet_list [`list`]: An object of type `list` returned by [`discovery_inductive()`] and containing 3 elements:
#' * petri net (list of places, transitions, flows)
#' * initial marking: place identifier of the initial marking.
#' * final marking: place identifier of the final marking.
#' @param multi_processing [lgl] (default [`FALSE`]): Disables if `FALSE`, enables if `TRUE` multiprocessing in inductive miner.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent.
#'  If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return precision metric.
#' @seealso \url{https://pm4py.fit.fraunhofer.de/static/assets/api/2.3.0/generated/pm4py.conformance.precision_alignments.html}
#'
#' @export
precision_alignments <- function(eventlog,
                                 marked_petrinet_list,
                                 multi_processing = FALSE,
                                 convert = TRUE) {
  UseMethod("precision_alignments")
}
#' @export
precision_alignments.eventlog <- function(eventlog,
                                          marked_petrinet_list,
                                          multi_processing = FALSE,
                                          # petrinet,
                                          # initial_marking,
                                          # final_marking,
                                          # activity_key,
                                          # timestamp_key,
                                          # case_id_key,
                                          convert = TRUE) {
  pm4py_conformance <- reticulate::import("pm4py.conformance", convert = convert)

  # prepare arguments for pm4py module
  py_log <- r_to_py(eventlog)
  py_pn <- as_py_value(marked_petrinet_list$petrinet)
  im <- as_pm4py_marking(marked_petrinet_list$initial_marking, py_pn)
  fm <- as_pm4py_marking(marked_petrinet_list$final_marking, py_pn)
  activity_key <- bupaR::activity_id(eventlog)
  timestamp_key <- bupaR::timestamp(eventlog)
  case_id_key <- bupaR::case_id(eventlog)
  multi_processing <- r_to_py(multi_processing)


  pm4py_conformance$precision_alignments(log = py_log, petri_net = py_pn,
                                         initial_marking = im,
                                         final_marking = fm,
                                         activity_key = activity_key,
                                         timestamp_key = timestamp_key,
                                         case_id_key = case_id_key,
                                         multi_processing = multi_processing)
}


# Diagnostics token-based replay ------------------------------------------
#' @title Apply __the token-based replay__ algorithm between a log and a process model
#'
#' @description Apply token-based replay for conformance checking analysis. The methods return the full token-based-replay diagnostics.
#'
#' @param eventlog A bupaR or PM4PY event log.
#' @param marked_petrinet_list [`list`]: An object of type `list` returned by [`discovery_inductive()`] and containing 3 elements:
#' * petri net (list of places, transitions, flows)
#' * initial marking: place identifier of the initial marking.
#' * final marking: place identifier of the final marking.
#' @param multi_processing [lgl] (default [`FALSE`]): Disables if `FALSE`, enables if `TRUE` multiprocessing in inductive miner.
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent.
#'  If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return The methods return the full token-based-replay diagnostics.
#' @seealso \url{https://pm4py.fit.fraunhofer.de/static/assets/api/2.3.0/generated/pm4py.conformance.conformance_diagnostics_token_based_replay.html}
#'
#' @export
conformance_token_based_replay <- function(eventlog,
                                           marked_petrinet_list,
                                           convert = TRUE) {
  UseMethod("conformance_token_based_replay")
}

#' @export
conformance_token_based_replay <- function(eventlog,
                                           marked_petrinet_list,
                                           # petrinet,
                                           # initial_marking,
                                           # final_marking,
                                           # activity_key,
                                           # timestamp_key,
                                           # case_id_key,
                                           convert = TRUE) {
  pm4py_conformance <- reticulate::import("pm4py.conformance", convert = convert)

  # prepare arguments for pm4py module
  py_log <- r_to_py(eventlog)
  py_pn <- as_py_value(marked_petrinet_list$petrinet)
  im <- as_pm4py_marking(marked_petrinet_list$initial_marking, py_pn)
  fm <- as_pm4py_marking(marked_petrinet_list$final_marking, py_pn)
  activity_key <- bupaR::activity_id(eventlog)
  timestamp_key <- bupaR::timestamp(eventlog)
  case_id_key <- bupaR::case_id(eventlog)

  pm4py_conformance$conformance_diagnostics_token_based_replay(log = py_log, petri_net = py_pn,
                                                               initial_marking = im,
                                                               final_marking = fm,
                                                               activity_key = activity_key,
                                                               timestamp_key = timestamp_key,
                                                               case_id_key = case_id_key)
}

