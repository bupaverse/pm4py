context("Discovery")

library(bupaR)

test_that("Inductive miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")
  patients %>% filter(registration_type == "complete") -> patients
  # net <- expect_silent(discover_inductive(patients, convert = FALSE))
  # expect_true("pm4py.objects.petri.petrinet.PetriNet" %in% class(net[0]))
  # expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[1]))
  # expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[2]))

  net <- discover_inductive(patients, convert = TRUE)
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

})


test_that("Alpha miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")
  patients %>% filter(registration_type == "complete") -> patients

  # net <- expect_silent(discover_alpha(patients, convert = FALSE))
  # expect_true("pm4py.objects.petri.petrinet.PetriNet" %in% class(net[0]))
  # expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[1]))
  # expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[2]))

  net <- discover_alpha(patients, convert = TRUE)
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

})
