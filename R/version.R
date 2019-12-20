#
# This file acts both as R source file as well as file defining environment variables that are sources in the CI/CD pipeline.
# TODO: separate this into two files where the R file reads from the environment variable file
#

# Whether the version is a package or a development version
PM4PY_DEVELOPMENT=FALSE

# PM4PY version, may be any pip identifier allowed, either:
#  pm4py==1.0.21
# or a Github link to a specific branch/tag/hash:
#  https://github.com/pm4py/pm4py-source/archive/develop.zip
PM4PY_VERSION="pm4py==1.2.6"

# PM4PY dependencies for development versions
PM4PY_DEPENDENCIES="https://raw.githubusercontent.com/pm4py/pm4py-source/1.2.6/requirements.txt"
