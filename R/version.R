#
# This file acts both as R source file as well as file defining environment variables that are sources in the CI/CD pipeline.
#

# Whether the version is a package or a development version
PM4PY_DEVELOPMENT=FALSE

# PM4PY dependencies for development versions
PM4PY_DEVELOPMENT_DEPENDENCIES="https://raw.githubusercontent.com/pm4py/pm4py-source/release/requirements.txt"

# PM4PY pip installer literal. For example:
#  pm4py==1.0.21
# or a Github link to a specific branch/tag/hash:
#  https://github.com/pm4py/pm4py-source/archive/develop.zip
PM4PY_PIP="pm4py==1.2.8"
