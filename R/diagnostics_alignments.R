#'  Apply the alignments algorithm between a log and a process model
#'
#' @description Alignment-based replay aims to find one of the best alignment between the trace and the model.
#'
#' @inheritParams discover_inductive
#' @inheritParams fitness_alignments
#' @return alignment diagnostics.
#'
#' @import reticulate
#'
#' @export
diagnostics_alignments <- function(log,
                                   marked_petrinet,
                                   multi_processing = FALSE,
                                   convert = TRUE) {
  UseMethod("diagnostics_alignments")
}
#' @describeIn diagnostics_alignment Compute aligments
#' @export
diagnostics_alignments.log <- function(log,
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


  alignments <- pm4py_conformance$conformance_diagnostics_alignments(py_log, py_pn, im, fm, # specs
                                                                     activity_key = activity_key,
                                                                     timestamp_key = timestamp_key,
                                                                     case_id_key = case_id_key,
                                                                     multi_processing = multi_processing)
  # py_pn, im, fm)
  cases <- cases(log)

  if(convert) {
    alignments %>%
      map(~.x[names(.x) != "alignment"]) %>%
      map(as_tibble) %>%
      bind_rows() -> costs

    move_to_tibble <- function(x) {
      x %>%
        map(~if(is.null(.x)) NA else .x) %>%
        as_tibble(.name_repair = function(names) c("log","model"))
    }

    alignments %>%
      map(~.x$alignment) %>% map(~
                                   map(.x, move_to_tibble) %>%
                                   bind_rows()) -> alignments
    costs[[case_id(log)]] <- cases[[case_id(log)]]

    costs %>%
      select(case_id(log), everything()) %>%
      mutate(alignment = alignments)
  }


}
