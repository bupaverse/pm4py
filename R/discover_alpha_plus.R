
#' @title Discover petrinet using Alpha plus Algorithm
#' @inheritParams discover_alpha
#' @return Marked petri net, i.e. a named list with elements `petrinet`, `initial_marking`, and `final_marking` or the original Python object.
#' @export
#'
discover_alpha_plus <- function(log, convert = TRUE) {
  UseMethod("discover_alpha_plus")
}

#' @describeIn discover_alpha_plus Discovery petri net based on event log
#' @export

discover_alpha_plus.eventlog <- function(log,
                                    convert = TRUE) {
  pm4py_discovery <- reticulate::import("pm4py", convert = convert)

  if(n_events(log) > n_activity_instances(log)) {
    cli::cli_warn("Activity instances with multiple events found in log: using only complete events.")

    log %>%
      filter(.data[[lifecycle_id(log)]] == "complete") -> log
  }
  log %>%
    mutate(across(activity_id(log), as.character)) -> log

  model <- pm4py_discovery$discover_petri_net_alpha_plus(as_py_value(log),
                                                    activity_key = activity_id(log),
                                                    timestamp_key = timestamp(log),
                                                    case_id_key = case_id(log))
  create_marked_PN(model[[1]], model[[2]], model[[3]])
}

