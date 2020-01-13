context("Bridge")

library(bupaR)
data("patients")
data("sepsis")
data("traffic_fines")
data("hospital_billing")

patrick::with_parameters_test_that("Event Log", {
  pm4py:::skip_if_no_pm4py()

  log <- log[[1]]

  py_log <- r_to_py(log)
  expect_true("pandas.core.frame.DataFrame" %in% class(py_log))

  r_log <- pm4py$objects$conversion$log$factory$apply(py_log,
                                                      parameters = default_parameters(log),
                                                      variant = "to_event_log")

  expect_true(nrow(r_log) == nrow(log))
  expect_equal(n_cases(log), length(unique(r_log[[bupaR::case_id(log)]])))
  expect_equal(n_activities(log), length(unique(r_log[[bupaR::activity_id(log)]])))
  expect_equal(n_resources(log), length(unique(r_log[[bupaR::resource_id(log)]])))
  expect_equal(length(log), length(r_log))

}, patrick::cases(
    "patients" = list(log = list(patients)),
    "sepsis" = list(log = list(sepsis)),
    "traffic_fines" = list(log = list(traffic_fines)),
    "hospital_billing" = list(log = list(hospital_billing))
   )
)



