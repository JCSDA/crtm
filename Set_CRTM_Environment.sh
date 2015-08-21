#!/bin/sh

# Shell script to set the values of the CRTM environment variables
# for the current subversion trunk, branch, or tag.
#
# This script should be *sourced* in the root directory of the
# aforementioned trunk, branch, or tag working copy.
#
# $Id$


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
export CRTM_CONFIG_ROOT="${CRTM_ROOT}/configuration"

alias crtmsrc="cd $CRTM_SOURCE_ROOT"
alias crtmfix="cd $CRTM_FIXFILE_ROOT"
alias crtmtest="cd $CRTM_TEST_ROOT"
alias crtmscripts="cd $CRTM_SCRIPTS_ROOT"
alias crtmext="cd $CRTM_EXTERNALS_ROOT"
alias crtmdoc="cd $CRTM_DOC_ROOT"
alias crtmval="cd $CRTM_VALIDATION_ROOT"
alias crtmconfig="cd $CRTM_CONFIG_ROOT"

echo "All CRTM environment variables now rooted at ${CRTM_ROOT}"


# Install scripts
echo "Installing ${CRTM_ROOT} based scripts..."
cd ${CRTM_SCRIPTS_ROOT}/shell
./crtm_install_scripts.sh
cd ${CRTM_ROOT}
