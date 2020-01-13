context("Discovery")

library(bupaR)

patrick::with_parameters_test_that("Inductive miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")

  net <- expect_silent(discovery_inductive(patients, variant = variant, convert = FALSE))
  expect_true("pm4py.objects.petri.petrinet.PetriNet" %in% class(net[0]))
  expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[1]))
  expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[2]))

  net <- expect_silent(discovery_inductive(patients, variant = variant, convert = TRUE))
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

}, patrick::cases(
    classic = list(variant = variant_inductive_imdfb()))
)


patrick::with_parameters_test_that("Alpha miner", {
  pm4py:::skip_if_no_pm4py()

  data("patients")

  net <- expect_silent(discovery_alpha(patients, variant = variant, convert = FALSE))
  expect_true("pm4py.objects.petri.petrinet.PetriNet" %in% class(net[0]))
  expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[1]))
  expect_true("pm4py.objects.petri.petrinet.Marking" %in% class(net[2]))

  net <- expect_silent(discovery_alpha(patients, variant = variant, convert = TRUE))
  expect_s3_class(net$petrinet, "petrinet")
  expect_length(net$initial_marking, 1)
  expect_length(net$final_marking, 1)

}, patrick::cases(
    classic = list(variant = variant_alpha_classic()),
    plus = list(variant = variant_alpha_plus()))
)
