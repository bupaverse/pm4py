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
