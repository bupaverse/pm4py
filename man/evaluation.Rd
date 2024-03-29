% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluation.R
\name{evaluation}
\alias{evaluation}
\alias{evaluation_all}
\alias{evaluation_precision}
\alias{variant_precision_etconformance}
\alias{evaluation_fitness}
\alias{variant_fitness_token_based}
\alias{variant_fitness_alignment_based}
\title{Calculates evaluation measures for a Petri nets and an Event Log}
\usage{
evaluation_all(
  eventlog,
  petrinet,
  initial_marking,
  final_marking,
  parameters = default_parameters(eventlog),
  convert = TRUE
)

evaluation_precision(
  eventlog,
  petrinet,
  initial_marking,
  final_marking,
  parameters = default_parameters(eventlog),
  variant = variant_precision_etconformance(),
  convert = TRUE
)

variant_precision_etconformance()

evaluation_fitness(
  eventlog,
  petrinet,
  initial_marking,
  final_marking,
  parameters = default_parameters(eventlog),
  variant = variant_fitness_token_based(),
  convert = TRUE
)

variant_fitness_token_based()

variant_fitness_alignment_based()
}
\arguments{
\item{eventlog}{A bupaR or PM4PY event log.}

\item{petrinet}{A bupaR or PM4PY Petri net.}

\item{initial_marking}{A R vector with the place identifiers of the initial marking or a PM4PY marking.
By default the initial marking of the bupaR Petri net will be used if available.}

\item{final_marking}{A R vector with the place identifiers of the final marking or a PM4PY marking.}

\item{parameters}{PM4PY alignment parameter.}

\item{convert}{\code{TRUE} to automatically convert Python objects to their R equivalent. If you pass \code{FALSE} you can do manual conversion using the \link[reticulate]{r-py-conversion} function.}

\item{variant}{Variant used
By default the \code{activity_key} from the bupaR event log is specified using \link{param_activity_key}.}
}
\value{
A \code{list} with all available evaluation measures.
}
\description{
Calculates evaluation measures for a Petri nets and an Event Log
}
