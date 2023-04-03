
#' @title Discover petrinet using Alpha Algorithm
#' @param log \code{\link{log}}: Object of class \code{\link{log}} or derivatives (\code{\link{grouped_log}}, \code{\link{eventlog}},
#' @param convert [logical] (default: [TRUE]): [TRUE] to automatically convert Python objects to their R equivalent.
#'  If you pass [FALSE] you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @export
#' @importFrom petrinetR create_marked_PN
#'
discover_alpha <- function(log, convert = TRUE) {
  UseMethod("discover_alpha")
}

#' @describeIn discover_alpha Discovery petri net based on event log
#' @export

discover_alpha.eventlog <- function(log,
                            convert = TRUE) {
  pm4py_discovery <- reticulate::import("pm4py", convert = convert)

  if(n_events(log) > n_activity_instances(log)) {
    cli::cli_warn("Activity instances with multiple events found in log: using only complete events.")

    log %>%
      filter(.data[[lifecycle_id(log)]] == "complete") -> log
  }
  log %>%
    mutate(across(activity_id(log), as.character)) -> log

  model <- pm4py_discovery$discover_petri_net_alpha(as_py_value(log),
                                                    activity_key = activity_id(log),
                                                    timestamp_key = timestamp(log),
                                                    case_id_key = case_id(log))
  create_marked_PN(model[[1]], model[[2]], model[[3]])
}

#' @describeIn discover_alpha Discovery petri net based on activity log
#' @export

discover_alpha.activitylog <- function(log,
                                    convert = TRUE) {
  pm4py_discovery <- reticulate::import("pm4py", convert = convert)

  if(n_events(log) > n_activity_instances(log)) {
    cli::cli_warn("Activity instances with multiple events found in log: using only complete events.")
  }
  log %>%
    mutate(across(activity_id(log), as.character)) -> log

  model <- pm4py_discovery$discover_petri_net_alpha(as_py_value(log),
                                                    activity_key = activity_id(log),
                                                    timestamp_key = "complete",
                                                    case_id_key = case_id(log))
  create_marked_PN(model[[1]], model[[2]], model[[3]])
}
