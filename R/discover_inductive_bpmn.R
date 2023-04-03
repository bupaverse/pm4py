
#' Discover BPMN models using Inductive Miner
#' @inheritParams discover_inductive
#' @return A BPMN object as defined by BPMNR
#' @export
#'
#' @import dplyr
#' @import bupaR

discover_inductive_bpmn <- function(log,
                                    multi_processing = FALSE,
                                    noise_threshold = 0,
                                    convert = TRUE) {
  UseMethod("discover_inductive_bpmn")
}
#' @export
discover_inductive_bpmn.log <- function(log,
                                        multi_processing = FALSE,
                                        noise_threshold = 0,
                                        convert = TRUE) {

  gatewayDirection <- NULL
  gatewayType <- NULL

  pm4py_inductive <- reticulate::import("pm4py.discovery", convert = convert)

  if(n_events(log) > n_activity_instances(log)) {
    cli::cli_warn("Activity instances with multiple events found in log: using only complete events.")


    if("eventlog" %in% class(log)) {
      log %>%
        filter(.data[[lifecycle_id(log)]] == "complete") -> log
    }
  }
  log %>%
    mutate(across(activity_id(log), as.character)) -> log


  multi_processing <- r_to_py(multi_processing)
  noise_threshold <- r_to_py(noise_threshold)
  activity_key <- bupaR::activity_id(log)
  timestamp_key <- bupaR::timestamp(log)
  case_id_key <- bupaR::case_id(log)

  model <- pm4py_inductive$discover_bpmn_inductive(r_to_py(log),
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

  # unlist(iterate(q, function(a) ensure_str(a$source$name)))
  #
  #
  # reticulate::dict(q, convert = F)
  #
  # py_builtins$set(unname(unlist(a_list)))

  bpmntmp$get_nodes()

  reticulate::iterate(bpmntmp$get_nodes(), function(p) ensure_str(p$name)) -> nodes_name
  reticulate::iterate(bpmntmp$get_nodes(), function(p) ensure_str(p$id)) -> nodes_id
  reticulate::iterate(bpmntmp$get_nodes(), function(p) (class(p))) -> nodes_class
  reticulate::iterate(bpmntmp$get_nodes(), function(p) if(stringr::str_detect(ensure_str(class(p)), "Gateway"))
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

  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$get_name())) -> arcs_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$get_id())) -> arcs_id
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$source$name)) -> arcs_from_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$source$id)) -> arcs_from_id

  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$target$name)) -> arcs_to_name
  reticulate::iterate(bpmntmp$get_flows(), function(p) ensure_str(p$target$id)) -> arcs_to_id

  tibble(id = arcs_id, name = arcs_name, sourceRef = arcs_from_id, targetRef = arcs_to_id) -> flows


  suppressWarnings({
    nodes %>%
      mutate(class =  forcats::fct_recode(class, "startEvent" = "StartEvent",
                                          "endEvent" = "EndEvent",
                                          "endEvent" = "NormalEndEvent",
                                          "task" = "Task",
                                          "exclusiveGateway" = "ExclusiveGateway",
                                          "inclusiveGateway" = "InclusiveGateway",
                                          "parallelGateway" = "ParallelGateway")) %>%
      mutate(gatewayDirection = forcats::fct_recode(gatewayDirection,
                                                    "Converging" = "CONVERGING",
                                                    "Diverging" = "DIVERGING")) %>%
      select(-gatewayType) -> nodes

  })

  events <- dplyr::filter(nodes, class %in% c("startEvent", "endEvent")) %>% dplyr::rename(objectType = class)
  nodes <- dplyr::filter(nodes, !class %in% c("startEvent", "endEvent")) %>% dplyr::rename(objectType = class)
  flows <- dplyr::mutate(flows, objectType = "sequenceFlow") %>%
    mutate(id = paste0("id_", id))

  bpmnR::create_bpmn(nodes, flows, events)
}
