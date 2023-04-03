#' Synchronous product Petri net
#'
#' Constructs the synchronous product net of two given Petri nets.
#'
#' @param pn1 First Petri net
#' @param im1 Initial marking of the first Petri net
#' @param fm1 Final marking of the first Petri net
#' @param pn2 Second Petri net
#' @param im2 Initial marking of the second Petri net
#' @param fm2 Final marking of the second Petri net
#' @param skip Symbol to be used as skip
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent. If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A Petri net.
#'
#' @import reticulate
#' @export
petrinet_synchronous_product <- function(pn1,
                                         im1,
                                         fm1,
                                         pn2,
                                         im2,
                                         fm2,
                                         skip = ">>",
                                         convert = TRUE) {

  lifecycle::deprecate_stop("2.0.0", "petrinet_synchronous_product")
  pm4py_sync <- import("pm4py.objects.petri.synchronous_product", convert = convert)

  pn1 <- as_py_value(pn1)
  pn2 <- as_py_value(pn2)

  sync_net <- pm4py_sync$construct(pn1,
                                   as_pm4py_marking(im1, pn1),
                                   as_pm4py_marking(fm1, pn1),
                                   pn2,
                                   as_pm4py_marking(im2, pn2),
                                   as_pm4py_marking(fm2, pn2),
                                   skip)

  prepare_pn_with_markings(sync_net, convert)
}

#' Check Workflow net property
#'
#' Checks if the Petri net is a Workflow net
#'
#'
#' @param pn Petri net
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent. If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A single logical
#'
#' @import reticulate
#' @export
petrinet_check_wfnet <- function(pn,
                                 convert = TRUE) {
  lifecycle::deprecate_stop("2.0.0", "petrinet_check_wfnet")

  pm4py_soundness <- import("pm4py.objects.petri.check_soundness", convert = convert)

  pn <- as_py_value(pn)

  pm4py_soundness$check_wfnet(pn)
}

#' Check Relaxed soundness property
#'
#' Checks if the Petri net is relaxed sound
#'
#' @param pn Petri net
#' @param im Initial marking of the Petri net (optional for workflow nets)
#' @param fm Final marking of the Petri net (optional for workflow nets)
#' @param convert `TRUE` to automatically convert Python objects to their R equivalent. If you pass `FALSE` you can do manual conversion using the \link[reticulate]{r-py-conversion} function.
#'
#' @return A single logical
#'
#' @import reticulate
#' @export
petrinet_check_relaxed_soundness <- function(pn,
                                             im = NULL,
                                             fm = NULL,
                                             convert = TRUE) {

  lifecycle::deprecate_stop("2.0.0", "petrinet_check_relaxed_soundness")

  pm4py_soundness <- import("pm4py.objects.petri.check_soundness", convert = convert)

  pn <- as_py_value(pn)

  if (petrinet_check_wfnet(pn)) {
    pm4py_soundness$check_relaxed_soundness_of_wfnet(pn)
  } else {
    stopifnot(!is.null(im))
    stopifnot(!is.null(fm))
    pm4py_soundness$check_relaxed_soundness_net_in_fin_marking(pn,
                                                               as_pm4py_marking(im, pn),
                                                               as_pm4py_marking(fm, pn))
  }

}
