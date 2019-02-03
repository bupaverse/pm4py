import pandas
import pm4py

from pm4py import util as pmutil
from pm4py.objects.log.log import EventLog, Trace
from pm4py.objects.log.util import xes

def get_trace_ids(log, parameters):
  if pmutil.constants.PARAMETER_CONSTANT_CASEID_KEY in parameters:
    caseid = parameters[pmutil.constants.PARAMETER_CONSTANT_CASEID_KEY]
  else:
    raise ValueError("Missing case id parameter")
  if isinstance(log, pandas.core.frame.DataFrame):
    return log[caseid].unique()
  if isinstance(log, pm4py.objects.log.log.EventStream) and (not isinstance(log, pm4py.objects.log.log.EventLog)):
    ids = set()
    for event in log:
      ids.add(event[caseid])
    return ids
  else:
    # should be an event log
    ids = []
    for trace in log:
      if (trace.attributes[xes.DEFAULT_TRACEID_KEY] != None):
        ids.append(trace.attributes[xes.DEFAULT_TRACEID_KEY])
    return ids


