prepare_pn_with_markings <- function(model, convert = FALSE) {
  if (convert) {
    if (inherits(model, "python.builtin.object")) {
      model <- py_to_r(model) # Python tuple was not auto-converted
    }
    model <- lapply(model, py_to_r)
    names(model) <- c("petrinet","initial_marking", "final_marking")
    model$petrinet$marking <- model$initial_marking
  }
  model
}

ensure_str <- function(x) {
  # Taking the Python string representation, but bupaR does not like ' in names
  # TODO fix in bupaR
  if (is.null(x)) {
    NA_character_
  } else {
    stringr::str_remove_all(py_str(as_py_value(x)), stringr::fixed("'"))
  }
}

as_r_value <- function(x) {

  if (inherits(x, "python.builtin.object"))
    py_to_r(x)
  else
    x
}

as_py_value <- function(x) {

  if (inherits(x, "python.builtin.object"))
    x
  else
    r_to_py(x)
}

skip_if_no_pm4py <- function() {
  if (!pm4py_available())
    testthat::skip("pm4py not available for testing")
}
