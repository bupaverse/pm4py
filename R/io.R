#' Write Petri net as PNML
#'
#' @param petrinet A bupaR or PM4PY Petri net.
#' @param file File name of the PNML file
#' @param initial_marking A R vector with the place identifiers of the initial marking or a PM4PY marking.
#' By default the initial marking of the bupaR Petri net will be used if available.
#' @param final_marking A R vector with the place identifiers of the final marking or a PM4PY marking.
#'
#' @export
write_pnml <- function(petrinet,
                       file,
                       initial_marking = NULL,
                       final_marking = NULL) {
  pm4py_export <- import("pm4py.objects.petri.exporter.pnml", convert = FALSE)

  if (is.null(initial_marking) && inherits(petrinet, "petrinet")) {
    initial_marking <- petrinet$marking
  }

  py_pn <- as_py_value(petrinet)

  if (!is.null(final_marking)) {
    py_final <- as_pm4py_marking(final_marking, py_pn)
  } else {
    py_final <- NULL
  }

  pm4py_export$export_net(petrinet = py_pn,
                          marking = as_pm4py_marking(initial_marking, py_pn),
                          output_filename = file,
                          final_marking = py_final)
  invisible(petrinet)
}
