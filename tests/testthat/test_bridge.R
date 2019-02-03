context("Bridge")

library(bupaR)

test_that("Event Log", {
  skip_if_no_pm4py()

  data("patients")

  py_log <- r_to_py(patients)
  expect_true("pandas.core.frame.DataFrame" %in% class(py_log))

  r_log <- pm4py$objects$conversion$log$factory$apply(py_log,
                                                  parameters = default_parameters(patients),
                                                  variant = "to_event_log")

  expect_true(nrow(r_log) == nrow(patients))
  expect_equal(n_cases(patients), length(unique(r_log$patient)))
  expect_equal(length(patients), length(r_log))

})



