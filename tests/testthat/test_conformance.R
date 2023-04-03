context("Conformance")

library(bupaR)

test_that("Alignment", {
  pm4py:::skip_if_no_pm4py()

  data("patients")

  patients <- patients %>% filter(registration_type == "complete")

  net <- discover_inductive(patients)

  # test two times for proper garbage collection
  res <- lapply(1:2, function(x) {
    a <- diagnostics_alignments(patients,
                              net,
                               convert = TRUE)
    numTraces <- n_cases(patients)
    numAlignments <- nrow(a)
    expect_equal(numAlignments, numTraces)
    return(a)
  })

  expect_length(res, 2)

})
