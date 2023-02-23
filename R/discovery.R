#' Petri net discovery algorithms
#'
#' PM4PY discovery algorithms that discover a Petri net and its initial and final marking. Currently the Inductive Miner and the Alpha Miner are implemented.
#'
#' @param eventlog A bupaR event log or an R data frame.
#' @param parameters A named list of PM4PY parameters (see \link{parameters}) as required by the discovery method.
#'  By default, if the `eventlog` is a bupaR event log, the `activity_key`, `timestamp_key`, and `caseid_key` are automatically determined.
#' @param variant The variant of the discovery algorithm to be used.
#' @param multi_processing [lgl] (default [`FALSE`]): Disables if `FALSE`, enables if `TRUE` multiprocessing in inductive miner.
#' @param noise_threshold [num] (default: 0): noise threshold.
#'  For Inductive Miner currently only `variant_inductive_imdfb` is supported.
#' @param convert TRUE to automatically convert Python objects to their R equivalent.
#'  If you pass FALSE you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A named list with elements `petrinet`, `initial_marking`, and `final_marking` or the original Python object.
#'
#' @examples
#' if (pm4py_available()) {
#'   library(eventdataR)
#'   data(patients)
#'
#'   # As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
#'   patients_completes <- patients[patients$registration_type == "complete", ]
#'
#'   net <- discovery_inductive(patients_completes, convert = T)
#'
#'   # Show details of the obtained bupaR Petri net
#'   print(net$petrinet)
#'
#'   # initial marking is a character vector
#'   print(net$initial_marking)
#'
#'   # final marking is a character vector
#'   print(net$final_marking)
#'
#'   # Petri net can be used with other bupaR functions
#'   petrinetR::render_PN(net$petrinet)
#'
#'   # Keep an unconverted PM4PY Petri net for use in other PM4PY functions
#'   py_net <- discovery_inductive(patients_completes, convert = FALSE)
#' }
#'
#' @name discovery
NULL

#' @rdname discovery
#' @export
discovery_inductive <- function(eventlog, # currently not for activitylog
                                multi_processing = FALSE,
                                noise_threshold = 0,
                                convert = TRUE) {
  UseMethod("discovery_inductive")
}
#' @export
discovery_inductive.eventlog <- function(eventlog, # currently not for activitylog
                                         multi_processing = FALSE,
                                         noise_threshold = 0,
                                         convert = TRUE) {
  pm4py_inductive <- reticulate::import("pm4py.discovery", convert = convert)

  eventlog[[bupaR:::activity_id_(eventlog)]] <- as.character(eventlog[[bupaR:::activity_id_(eventlog)]])

  multi_processing <- r_to_py(multi_processing)
  noise_threshold <- r_to_py(noise_threshold)
  activity_key <- bupaR::activity_id(eventlog)
  timestamp_key <- bupaR::timestamp(eventlog)
  case_id_key <- bupaR::case_id(eventlog)
  model <- pm4py_inductive$discover_petri_net_inductive(r_to_py(eventlog),
                                                        multi_processing = multi_processing,
                                                        noise_threshold = noise_threshold,
                                                        activity_key = activity_key,
                                                        timestamp_key = timestamp_key,
                                                        case_id_key = case_id_key)




  create_marked_PN(model[[1]], model[[2]], model[[3]])
}

#' @rdname discovery
#' @export
discovery_inductive_bpmn <- function(eventlog, # currently not for activitylog
                                     multi_processing = FALSE,
                                     noise_threshold = 0,
                                     convert = TRUE) {
  UseMethod("discovery_inductive_bpmn")
}
#' @export
discovery_inductive_bpmn.eventlog <- function(eventlog, # currently not for activitylog
                                              multi_processing = FALSE,
                                              noise_threshold = 0,
                                              convert = TRUE) {
  pm4py_inductive <- reticulate::import("pm4py.discovery", convert = convert)

  eventlog[[bupaR:::activity_id_(eventlog)]] <- as.character(eventlog[[bupaR:::activity_id_(eventlog)]])

  multi_processing <- r_to_py(multi_processing)
  noise_threshold <- r_to_py(noise_threshold)
  activity_key <- bupaR::activity_id(eventlog)
  timestamp_key <- bupaR::timestamp(eventlog)
  case_id_key <- bupaR::case_id(eventlog)
  model <- pm4py_inductive$discover_bpmn_inductive(r_to_py(eventlog),
                                                   multi_processing = multi_processing,
                                                   noise_threshold = noise_threshold,
                                                   activity_key = activity_key,
                                                   timestamp_key = timestamp_key,
                                                   case_id_key = case_id_key) -> bpmntmp

  bpmntmp$get_nodes() # tasks/nodes: data.frame of all tasks and their attributes
  bpmntmp$get_flows() # sequencyFlows: data.frame of all sequence flows and their attributes
  bpmntmp$Gateway() # gateways: data.frame of all gateways and their attributes
  bpmntmp$StartEvent() # startEvent: data.frame containing the start event and its attributes
  bpmntmp$EndEvent() # endEvent: data.frame containing the end event and its attributes

  #
  # unlist(iterate(q, function(a) ensure_str(a$source$name)))
  #
  #
  # reticulate::dict(q, convert = F)
  #
  # py_builtins$set(unname(unlist(a_list)))

  bpmntmp$get_nodes()


  reticulate::iterate(bpmntmp$get_nodes(), function(p) pm4py:::ensure_str(p$name)) -> nodes_name
  reticulate::iterate(bpmntmp$get_nodes(), function(p) pm4py:::ensure_str(p$id)) -> nodes_id
  reticulate::iterate(bpmntmp$get_nodes(), function(p) (class(p))) -> nodes_class
  reticulate::iterate(bpmntmp$get_nodes(), function(p) if(stringr::str_detect(pm4py:::ensure_str(class(p)), "Gateway"))
    p$get_gateway_direction()) -> nodes_directions

  tibble(name = nodes_name, id =  nodes_id, nodes_class, gatewayDirection = nodes_directions) %>%
    mutate(class = str_remove(map_chr(nodes_class, ~.x[[1]]), "pm4py.objects.bpmn.obj."),
           gatewayDirection = map(gatewayDirection, as.character)) %>%
    tidyr::unnest(gatewayDirection, keep_empty = T) %>%
    mutate(gatewayDirection = str_remove(gatewayDirection, "Direction."),
           gatewayType = class) %>%
    select(-nodes_class) -> nodes


  # reticulate::iterate(bpmntmp$get_nodes(), function(p) if(stringr::str_detect(pm4py:::ensure_str(class(p)), "Gateway"))
  # 	p$get_out_arcs()) -> nodes_id

  reticulate::iterate(bpmntmp$get_flows(), function(p) pm4py:::ensure_str(p$get_name())) -> arcs_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) pm4py:::ensure_str(p$get_id())) -> arcs_id
  reticulate::iterate(bpmntmp$get_flows(), function(p) pm4py:::ensure_str(p$source$name)) -> arcs_from_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) pm4py:::ensure_str(p$source$id)) -> arcs_from_id

  reticulate::iterate(bpmntmp$get_flows(), function(p) pm4py:::ensure_str(p$target$name)) -> arcs_to_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) pm4py:::ensure_str(p$target$id)) -> arcs_to_id

  tibble(id = arcs_id, name = arcs_name, sourceRef = arcs_from_id, targetRef = arcs_to_id) -> flows


  bpmnR::create_bpmn(tasks = nodes %>% filter(class == "Task") %>% select(name, id),
                     sequenceFlows = flows,
                     gateways = nodes %>% filter(class == "ExclusiveGateway"),
                     startEvent = nodes %>% filter(class == "StartEvent") %>% select(name, id),
                     endEvent = nodes %>% filter(class == "NormalEndEvent") %>% select(name, id))
}

#' @rdname discovery
#' @export
variant_inductive_imdfb <- function() {
  pm4py$algo$discovery$inductive$factory$IMDFB
}


#' @rdname discovery
#' @export
variant_inductive_only_dfg <- function() {
  .Deprecated("variant_inductive_imdfb")
  pm4py$algo$discovery$inductive$factory$IMDFB
}

#' @rdname discovery
#' @export
discovery_alpha <- function(eventlog,
                            parameters = default_parameters(eventlog),
                            variant = variant_alpha_classic(),
                            convert = TRUE) {
  pm4py_alpha <- reticulate::import("pm4py.algo.discovery.alpha.factory", convert = convert)
  model <- pm4py_alpha$apply(as_py_value(eventlog),
                             parameters = parameters,
                             variant = variant)
  prepare_pn_with_markings(model, convert)
}

#' @rdname discovery
#' @export
variant_alpha_classic <- function() {
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_CLASSIC
}

#' @rdname discovery
#' @export
variant_alpha_plus <- function() {
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_PLUS
}
