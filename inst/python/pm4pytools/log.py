from pm4py.objects.log.log import TraceLog, Trace
from pm4py.objects.log.util import xes

def get_trace_ids(log):
  ids = []
  for trace in log:
    if (trace.attributes[xes.DEFAULT_TRACEID_KEY] != None):
      ids.append(trace.attributes[xes.DEFAULT_TRACEID_KEY])
  return ids
