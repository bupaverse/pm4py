# R Interface to the PM4Py Process Mining Library

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pm4py)](https://cran.r-project.org/package=pm4py)
[![Travis-CI Build Status](https://travis-ci.org/bupaverse/pm4py.svg?branch=master)](https://travis-ci.org/bupaverse/pm4py)

The goal of the R package 'pm4py' is to provide a bridge between [bupaR](https://www.bupar.net/) and the Python library [PM4Py](http://pm4py.org/).

## Installation

You can install the development version of pm4py with:

``` r
remotes::install_github("bupaverse/pm4py")
```

Then, automatically install the pm4py package in a virtual or Conda environment:
``` r
pm4py::install_pm4py()
```

See the 'reticulate' documentation for more information on the available options or how to specify an existing Python environment: 
https://rstudio.github.io/reticulate/

## PM4Py Version

To help reproducibility and reduce the number of regressions due to API changes in PM4Py, this package is built against a fixed PM4Py version that is defined in the file `R/version.R`. We also adopt the versioning schema of the PM4Py project for this R package. So, the R package version `1.1.19` will install the PM4Py version `1.1.19`. In case of fixes required to the R package itself, for example, for bugs or adopting new features, we will add a suffix `-rev` to the version to indicate the change. Of course, nothing prevents you from manually overriding the synchronisation between the PM4Py version and the R PM4Py package version.


## Example

``` r
library(pm4py)

# Most of the data structures are converted in their bupaR equivalents
library(bupaR)

# As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
patients_completes <- patients[patients$registration_type == "complete", ]

# Discovery with Inductive Miner
pn <- discovery_inductive(patients_completes)

# This results in an auto-converted bupaR Petri net and markings
str(pn)
class(pn$petrinet)

# Render with bupaR
render_PN(pn$petrinet)

# Render with  PM4PY and DiagrammeR
library(DiagrammeR)
viz <- reticulate::import("pm4py.visualization.petrinet")

# Convert back to Python
py_pn <- r_to_py(pn$petrinet)
class(py_pn)

# Render to DOT with PMP4Y
dot <- viz$factory$apply(py_pn)$source
grViz(diagram = dot)

# Compute alignment
alignment <- conformance_alignment(patients_completes, pn$petrinet, pn$initial_marking, pn$final_marking)

# # Alignment is returned in long format as data frame
head(alignment)

# Evaluate model quality
quality <- evaluation_all(patients_completes, pn$petrinet, pn$initial_marking, pn$final_marking)
```
