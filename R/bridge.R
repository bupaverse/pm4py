#' @importFrom reticulate r_to_py
#' @export
reticulate::r_to_py

#' @importFrom reticulate py_to_r
#' @export
reticulate::py_to_r

#' @export
#' @importFrom reticulate r_to_py
r_to_py.log <- function(x, convert = FALSE) {
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

  pm4py_petrinet <- import("pm4py.objects.petri_net.obj", convert = convert)
  py_builtins <- import_builtins(convert = convert)

  t_list <- Map(function(name, label) {
    if (is.na(label)) {
      label <- NULL
    }
    pm4py_petrinet$PetriNet$Transition(name, label)
  }, x$transitions$id, x$transitions$label)

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
#' @importFrom reticulate r_to_py
r_to_py.bpmn <- function(x, convert = F) {

  pm4py_bpmn <- reticulate::import("pm4py.objects.bpmn.obj", convert = convert)

  # gateways
  Map(function(id, name, gateway_direction) {
    pm4py_bpmn$BPMN$Gateway(id, name, gateway_direction)
  }, x$gateways$id, x$gateways$name, x$gateways$gatewayDirection) -> gateways

  # tasks
  Map(function(id, name) {
    pm4py_bpmn$BPMN$Task(id, name)
  }, x$tasks$id, x$task$name) -> tasks

  # startEvent
  Map(function(id, name) {
    pm4py_bpmn$BPMN$StartEvent(id, name)
  }, x$startEvent$id, x$startEvent$name) -> startEvent

  # endEvent
  Map(function(id, name) {
    pm4py_bpmn$BPMN$EndEvent(id, name)
  }, x$endEvent$id, x$endEvent$name) -> endEvent

  # sequenceFlows
  Map(function(sourceRef, targetRef, id, name) {

    # find source
    if(sourceRef %in% names(gateways))
      source <- gateways[[sourceRef]]
    else if (sourceRef %in% names(startEvent)) {
      source <- startEvent[[sourceRef]]
    }
    else if(sourceRef %in% names(endEvent)) {
      source <- endEvent[[sourceRef]]
    }
    else {
      source <- tasks[[sourceRef]]
    }

    # find target
    if(targetRef %in% names(gateways))
      target <- gateways[[targetRef]]
    else if (targetRef %in% names(startEvent)) {
      target <- startEvent[[targetRef]]
    }
    else if(targetRef %in% names(endEvent)) {
      target <- endEvent[[targetRef]]
    }
    else {
      target <- tasks[[targetRef]]
    }

    # map
    pm4py_bpmn$BPMN$SequenceFlow(source = source, target = target, id = id, name = name)
  }, x$sequenceFlows$sourceRef, x$sequenceFlows$targetRef, x$sequenceFlows$id, x$sequenceFlows$name) -> sequenceFlows

  unname(unlist(tasks)) -> tasks
  unname(unlist(sequenceFlows)) -> flows
  unname(unlist(gateways)) -> gateways
  unname(unlist(startEvent)) -> startEvent
  unname(unlist(endEvent)) -> endEvent
  c(tasks, gateways, startEvent, endEvent) -> nodes

  pm4py_bpmn$BPMN(#process_id = "1", name = "A",
    nodes = nodes,
    flows = flows) -> py_x

  return(py_x)
  # return(list(tasks = tasks, gateways = gateways, sequenceFlows = sequence_flows, startEvent = start_events, endEvent = end_events))
}

#' @export
#' @importFrom reticulate iterate
#' @importFrom reticulate py_to_r
py_to_r.pm4py.objects.petri_net.obj.PetriNet <- function(x) {

  # Make sure that labels are strings since PM4PY sometimes uses tuples or other objects
  # Also, replace NULL by NA to avoid issues with R removing elements

  place_ids <- unlist(iterate(x$places, function(p) ensure_str(p$name)))
  transition_ids <- unlist(iterate(x$transitions, function(t) ensure_str(t$name)))
  transition_labels <- unlist(iterate(x$transitions, function(t) ensure_str(t$label)))

  arcs_from <- unlist(iterate(x$arcs, function(a) ensure_str(a$source$name)))
  arcs_to <- unlist(iterate(x$arcs,function(a) ensure_str(a$target$name)))

  flows <- data.frame(from = arcs_from, to = arcs_to, stringsAsFactors = F)
  transitions <- data.frame(label = transition_labels, id = transition_ids)
  places <- data.frame(id = place_ids, label = place_ids)

  pn <- petrinetR::create_PN(places, transitions, flows) 

  pn
}

#' @export
#' @importFrom reticulate iterate
#' @importFrom reticulate py_to_r
py_to_r.pm4py.objects.petri_net.obj.Marking <- function(x) {
  iterate(x$elements(), function(p) ensure_str(p$name))
}

#' Convert to a PM4Py marking
#'
#' Converts a character vector of place identifiers to a PM4Py marking object.
#'
#' @param x A character vector with (possible duplicate) place identifiers.
#' @param petrinet A PM4Py Petri net.
#' @return PM4Py marking object
#' @export
as_pm4py_marking <- function(x, petrinet) {

  if (inherits(x, "pm4py.objects.petri_net.data_petri_nets.data_marking.DataMarking")) {
    return(x)
  }

  pm4py_petrinet <- import("pm4py.objects.petri_net.obj", convert = FALSE)
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
