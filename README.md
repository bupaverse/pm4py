# pm4py

The goal of pm4py is to provide a bridge to the Python library PM4PY developed at: http://pm4py.org/

## Installation

You can install the development version of pm4py with:

``` r
remotes::install_github("fmannhardt/pm4py")
```

## Example

``` r
library(pm4py)
library(bupaR)

#
# Discovery with Inductive Miner
#
pn <- discovery_inductive(patients)

# result is auto converted to petrinetR object plus markings
str(pn)
class(pn$petrinet)

# Render with bupaR
render_PN(pn$petrinet)

#
# Render with  PM4PY and DiagrammeR
#
library(DiagrammeR)
viz <- import("pm4py.visualization.petrinet")

# Convert back to Python
py_pn <- r_to_py(pn$petrinet)
class(py_pn)

# Render to DOT with PMP4Y
dot <- viz$factory$apply(py_pn)$source
grViz(diagram = dot)
```

