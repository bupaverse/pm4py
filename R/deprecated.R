#' @title Deprecated functions
#'
#' @param eventlog Eventlog object
#' @param parameters Parameters
#' @param variant Variant
#' @inheritParams discover_inductive
#'
#' @name deprecated
#' @export
discovery_alpha <- function(eventlog,
                            parameters = default_parameters(eventlog),
                            variant = variant_alpha_classic(),
                            convert = TRUE) {
  lifecycle::deprecate_warn(when = "2.0.0", what = "discovery_alpha()", with = "discover_alpha()")


  discover_alpha(eventlog, convert = convert)

}

#' @rdname deprecated
#' @export
variant_alpha_classic <- function() {
  lifecycle::deprecate_warn(when = "2.0.0", what = "variant_alpha_classic()", with = "discover_alpha()")
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_CLASSIC
}

#' @rdname deprecated
#' @export
variant_alpha_plus <- function() {
  lifecycle::deprecate_warn(when = "2.0.0", what = "variant_alpha_plus()", with = "discover_alpha_plus()")
  pm4py$algo$discovery$alpha$factory$ALPHA_VERSION_PLUS
}

#' @rdname deprecated
#' @export
variant_inductive_imdfb <- function() {
  lifecycle::deprecate_stop("2.0.0", "variant_inductive_only_dfg()")
  pm4py$algo$discovery$inductive$factory$IMDFB
}


#' @rdname deprecated
#' @export
variant_inductive_only_dfg <- function() {
  lifecycle::deprecate_stop("2.0.0", "variant_inductive_only_dfg()")
  pm4py$algo$discovery$inductive$factory$IMDFB
}
