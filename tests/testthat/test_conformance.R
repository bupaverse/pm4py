context("Conformance")

library(bupaR)

test_that("Alignment", {
  skip_if_no_pm4py()

  data("patients")

  net <- discovery_inductive(patients)

  res <- lapply(1:2, function(x) {
    a <- conformance_alignment(patients,
                               net$petrinet,
                               net$initial_marking,
                               net$final_marking,
                               convert = TRUE)
    numTraces <- n_cases(patients)
    numAlignments <- length(unique(a$case_id))
    expect_equal(numAlignments, numTraces)
    return(a)
  })
  expect_length(res, 2)

})
