#' Petri net discovery algorithms
#'
#' PM4PY discovery algorithms that discover a Petri net and its initial and final marking. Currently the Inductive Miner and the Alpha Miner are implemented.
#'
#' @param multi_processing [logical] (default [`FALSE`]): Disables if `FALSE`, enables if `TRUE` multiprocessing in inductive miner.
#' @param noise_threshold [numeric] (default: 0): noise threshold.
#'  For Inductive Miner currently only `variant_inductive_imdfb` is supported.
#' @inheritParams discover_alpha
#' @return A named list with elements `petrinet`, `initial_marking`, and `final_marking` or the original Python object.
#'
#'
#' @export
discover_inductive <- function(log, # currently not for activitylog
                                multi_processing = FALSE,
                                noise_threshold = 0,
                                convert = TRUE) {
  UseMethod("discover_inductive")
}

#' @describeIn discover_inductive Discover Inductive Miner model based on event log
#' @export
discover_inductive.log <- function(log,
                                         multi_processing = FALSE,
                                         noise_threshold = 0,
                                         convert = TRUE) {
  pm4py_inductive <- reticulate::import("pm4py.discovery", convert = convert)


  if(n_events(log) > n_activity_instances(log)) {
    cli::cli_warn("Activity instances with multiple events found in log: using only complete events.")

    log %>%
      filter(.data[[lifecycle_id(log)]] == "complete") -> log
  }
  log %>%
    mutate(across(activity_id(log), as.character)) -> log


  multi_processing <- r_to_py(multi_processing)
  noise_threshold <- r_to_py(noise_threshold)
  model <- pm4py_inductive$discover_petri_net_inductive(r_to_py(log),
                                                        multi_processing = multi_processing,
                                                        noise_threshold = noise_threshold,
                                                        activity_key = activity_id(log),
                                                        timestamp_key = timestamp(log),
                                                        case_id_key = case_id(log))




  create_marked_PN(model[[1]], model[[2]], model[[3]])
}
