
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

as_pm4py_marking <- function(x, petrinet) {

  if (inherits(x, "pm4py.objects.petri.petrinet.Marking")) {
    return(x)
  }

  pm4py_petrinet <- import("pm4py.objects.petri.petrinet", convert = FALSE)
  marking <- pm4py_petrinet$Marking()

  found <- rep(FALSE, length(x))
  py <- import_builtins()
  iter <- py$iter(petrinet$places)

  while (TRUE) {
    item <- iter_next(iter)
    if (is.null(item))
      break

    loc <- which(x == item$name)
    if (any(loc)) {
      found[loc] <- TRUE
      marking[item] = length(loc)
    }
  }

  if (all(found)) {
    return(marking)
  } else {
    stop(paste0("Places ",
                paste0(x[!found], collapse = ","),
                " not found in ",
                py_str(petrinet$places)))
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
  have_pm4py <- py_module_available("pm4py")
  if (!have_pm4py)
    testthat::skip("pm4py not available for testing")
}
