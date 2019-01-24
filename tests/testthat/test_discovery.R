context("Discovery")

library(bupaR)

test_that("Inductive miner", {
  skip_if_no_pm4py()

  data("patients")

  net <- expect_silent(discovery_inductive(patients, convert = FALSE))
  expect_true("pm4py.objects.petri.petrinet.PetriNet" %in% class(net[0]))
  expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[1]))
  expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[2]))

  net <- expect_silent(discovery_inductive(patients, convert = TRUE))
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

})
