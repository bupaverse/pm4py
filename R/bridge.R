#' @importFrom reticulate r_to_py
#' @export
reticulate::r_to_py

#' @importFrom reticulate py_to_r
#' @export
reticulate::py_to_r

#' @export
#' @importFrom reticulate r_to_py
r_to_py.eventlog <- function(x, convert = FALSE) {
  df <- as.data.frame(x)
  # fix for https://github.com/rstudio/reticulate/issues/389
  #i <- sapply(df, is.factor)
  #df[i] <- lapply(df[i], as.character)
  r_to_py(df, convert = convert)
}

#' @export
#' @importFrom reticulate py_to_r
py_to_r.pm4py.objects.log.log.EventLog <- function(x) {
  variant_to_dataframe <- pm4py$objects$conversion$log$constants$TO_DATAFRAME
  df <- pm4py$objects$conversion$log$factory$apply(x, variant = variant_to_dataframe)
  df[ , !names(df) %in% c("case:concept:name")]
}

#' @export
#' @importFrom reticulate py_to_r
py_to_r.pm4py.objects.log.log.TraceLog <- function(x) {
  #TODO: recover the complete bupaR event log which requires:
  # - case identifier (could come from 'case:concept:name')
  # - activity identifier (classifier? but which one?)
  # - activity instance identifier (auto generate?)
  # ....
  variant_to_dataframe <- pm4py$objects$conversion$log$constants$TO_DATAFRAME
  df <- pm4py$objects$conversion$log$factory$apply(x, variant = variant_to_dataframe)
  df[ , !names(df) %in% c("case:concept:name")]
}

#' @export
#' @importFrom reticulate r_to_py
r_to_py.petrinet <- function(x, convert = FALSE) {

  pm4py_petrinet <- import("pm4py.objects.petri.petrinet", convert = convert)
  py_builtins <- import_builtins(convert = convert)

  if ("label" %in% names(x$transitions)) {
    t_labels <- x$transitions$label
  } else {
    t_labels <- rep(NA, nrow(x$transitions))
  }

  t_list <- Map(function(name, label) {
    if (is.na(label)) {
      label <- NULL
    }
    pm4py_petrinet$PetriNet$Transition(name, label)
  }, x$transitions$id, t_labels)

  p_list <- Map(function(p) pm4py_petrinet$PetriNet$Place(p), x$places$id)

  a_list <- Map(function(from, to) {
    if (from %in% names(t_list)) {
      source <- t_list[[from]]
      target <- p_list[[to]]
    } else {
      source <- p_list[[from]]
      target <- t_list[[to]]
    }

    a <- pm4py_petrinet$PetriNet$Arc(source, target)

    source$out_arcs$add(a)
    target$in_arcs$add(a)

    return(a)

  }, x$flows$from, x$flows$to)

  pm4py_petrinet$PetriNet(
    places = unname(unlist(p_list)),
    transitions = unname(unlist(t_list)),
    arcs = py_builtins$set(unname(unlist(a_list)))
  )
}

#' @export
#' @importFrom reticulate iterate
#' @importFrom reticulate py_to_r
py_to_r.pm4py.objects.petri.petrinet.PetriNet <- function(x) {

  places <- unlist(iterate(x$places, function(p) ensure_str(p$name)))
  transitions <- unlist(iterate(x$transitions, function(t) ensure_str(t$name)))

  arcs_from <- unlist(iterate(x$arcs, function(a) ensure_str(a$source$name)))
  arcs_to <- unlist(iterate(x$arcs,function(a) ensure_str(a$target$name)))

  flows <- data.frame(from = arcs_from, to = arcs_to,
                      stringsAsFactors = F)

  pn <- petrinetR::create_PN(places, transitions, flows,
                             c()) # pm4py Petri net does not know about marking

  # Make sure that labels are strings since PM4PY sometimes uses tuples or other objects
  # Also, replace NULL by NA to avoid issues with R removing elements
  pn$transitions$label <- iterate(x$transitions, function(t) ensure_str(t$label))

  pn
}

#' @export
#' @importFrom reticulate iterate
#' @importFrom reticulate py_to_r
py_to_r.pm4py.objects.petri.petrinet.Marking <- function(x) {
  iterate(x$elements(), function(p) ensure_str(p$name))
}
