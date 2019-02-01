#' @import reticulate
evaluation_token_replay <- function(eventlog,
                                  petrinet,
                                  initial_marking,
                                  final_marking,
                                  parameters = param_activity_key(bupaR::activity_id(eventlog)),
                                  variant = variant_token_based(),
                                  convert = TRUE) {
  pm4py_evaluation <- import("pm4py.evaluation.factory", convert = convert)

  py_pn <- as_py_value(petrinet)
  py_log <- as_py_value(eventlog)

  # bugfix for https://github.com/pm4py/pm4py-source/issues/42
  #if ("pm4py:param:activity_key" %in% names(parameters)) {
  #  parameters <- c(parameters, list("activity_key" = parameters[["pm4py:param:activity_key"]]))
  #}

  #bug in PM4PY

  m <- pm4py_evaluation$apply(log = py_log,
                              net = py_pn,
                              initial_marking = as_pm4py_marking(initial_marking, py_pn),
                              final_marking = as_pm4py_marking(final_marking, py_pn),
                              parameters = parameters,
                              variant = variant)
  m
}

variant_token_based <- function() {
  pm4py$evaluation$factory$TOKEN_BASED
}

