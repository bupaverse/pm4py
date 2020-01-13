context("Conformance")

library(bupaR)

patrick::with_parameters_test_that("Alignment", {
  pm4py:::skip_if_no_pm4py()

  data("patients")

  net <- discovery_inductive(patients)

  # test two times for proper garbage collection
  res <- lapply(1:2, function(x) {
    a <- conformance_alignment(patients,
                               net$petrinet,
                               net$initial_marking,
                               net$final_marking,
                               variant = variant,
                               convert = TRUE)
    numTraces <- n_cases(patients)
    numAlignments <- length(unique(a$case_id))
    expect_equal(numAlignments, numTraces)
    return(a)
  })

  expect_length(res, 2)

}, patrick::cases(
    dijkstra = list(variant = variant_dijkstra_no_heuristics()),
    astar = list(variant = variant_state_equation_a_star()))
)
