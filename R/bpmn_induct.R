#
# tmp_pm4py <- reticulate::import("pm4py", convert = T)
#
# # prep
# log <- log[log$registration_type == "complete", ]
# log[[bupaR:::activity_id_(log)]] <- as.character(log[[bupaR:::activity_id_(log)]])
# activity_key <- bupaR::activity_id(log)
# timestamp_key <- bupaR::timestamp(log)
# case_id_key <- bupaR::case_id(log)
#
# # functie
# tmp_pm4py$discover_bpmn_inductive(log = r_to_py(log),
#                                   activity_key = activity_key, timestamp_key = timestamp_key, case_id_key = case_id_key) -> bpmntmp
#
# q <- bpmntmp$get_nodes() # tasks/nodes: data.frame of all tasks and their attributes
# w <- bpmntmp$get_flows() # sequencyFlows: data.frame of all sequence flows and their attributes
# bpmntmp$Gateway() # gateways: data.frame of all gateways and their attributes
# bpmntmp$StartEvent() # startEvent: data.frame containing the start event and its attributes
# bpmntmp$EndEvent() # endEvent: data.frame containing the end event and its attributes
#
# #
# # unlist(iterate(q, function(a) ensure_str(a$source$name)))
# #
# #
# # reticulate::dict(q, convert = F)
# #
# # py_builtins$set(unname(unlist(a_list)))
# #
#
#


