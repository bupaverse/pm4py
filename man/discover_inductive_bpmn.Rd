% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discover_inductive_bpmn.R
\name{discover_inductive_bpmn}
\alias{discover_inductive_bpmn}
\title{Discover BPMN models using Inductive Miner}
\usage{
discover_inductive_bpmn(
  log,
  multi_processing = FALSE,
  noise_threshold = 0,
  convert = TRUE
)
}
\arguments{
\item{log}{\code{\link{log}}: Object of class \code{\link{log}} or derivatives (\code{\link{grouped_log}}, \code{\link{eventlog}},}

\item{multi_processing}{\link{logical} (default \code{\link{FALSE}}): Disables if \code{FALSE}, enables if \code{TRUE} multiprocessing in inductive miner.}

\item{noise_threshold}{\link{numeric} (default: 0): noise threshold.
For Inductive Miner currently only \code{variant_inductive_imdfb} is supported.}

\item{convert}{\link{logical} (default: \link{TRUE}): \link{TRUE} to automatically convert Python objects to their R equivalent.
If you pass \link{FALSE} you can do manual conversion using the \link[reticulate]{r-py-conversion} function.}
}
\value{
A BPMN object as defined by BPMNR
}
\description{
Discover BPMN models using Inductive Miner
}
