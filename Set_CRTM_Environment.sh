#!/bin/sh

# Set current directory as new CRTM root.
export CRTM_ROOT=${PWD}

# Construct the new CRTM environment variables
export CRTM_SOURCE_ROOT="${CRTM_ROOT}/src"
export CRTM_FIXFILE_ROOT="${CRTM_ROOT}/fix"
export CRTM_TEST_ROOT="${CRTM_ROOT}/test"
export CRTM_EXTERNALS_ROOT="${CRTM_ROOT}/externals"
export CRTM_SCRIPTS_ROOT="${CRTM_ROOT}/scripts"
export CRTM_DOC_ROOT="${CRTM_ROOT}/doc"
export CRTM_VALIDATION_ROOT="${CRTM_ROOT}/validation"

echo "All CRTM environment variables now rooted at ${CRTM_ROOT}"
