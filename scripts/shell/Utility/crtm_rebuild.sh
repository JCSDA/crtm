#!/bin/bash

# Script to perform a clean rebuild of the CRTM library
#
# $Id$

script_id()
{
  REVISION='$Revision$'
  LAST_CHANGED_DATE='$LastChangedDate$'
  echo
  echo "${SCRIPT_NAME} ${REVISION} ${LAST_CHANGED_DATE}"
  echo " "`date`
  echo " Support email: NCEP.List.EMC.JCSDA_CRTM.Support@noaa.gov"
}

usage()
{
  echo
  echo " Usage: crtm_rerebuild.sh [-dhx] [-c compiler]"
  echo
  echo "   Script to perform a clean rebuild of the CRTM library."
  echo
  echo " Options:"
  echo "   -d"
  echo "         Use DEBUG compilation switches. Default is PRODUCTION."
  echo
  echo "   -h"
  echo "         Print this message"
  echo
  echo "   -x"
  echo "         Turn on execution tracing"
  echo
  echo "   -c compiler"
  echo "         Use this option to specify the compiler. Valid compilers are:"
  echo "           * gfortran [DEFAULT]"
  echo "           * ifort"
  echo "           * pgf95"
  echo "           * g95"
  echo "           * xlf2003"
  echo
}

error_message()
{
  MESSAGE=$1
  echo >&2
  echo "  *********************" >&2
  echo "  ${SCRIPT_NAME}(ERROR): ${MESSAGE}" >&2
  echo "  *********************" >&2
}

compiler_exists () {
  command -v $1 >/dev/null 2>&1
}

########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# Setup
SCRIPT_NAME=`basename $0`
# ...Defintiions
SUCCESS=0
FAILURE=1
# ..Define defaults
DEBUG=""
COMPILER=${FC}
echo "Compiler:" $COMPILER


# Parse command line options
while getopts :dhxc: OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    d)  DEBUG="-debug" ;;
    h)  usage | more; exit ${SUCCESS} ;;
    x)  script_id; set -x ;;
    c)  COMPILER="${OPTARG}" ;;
    \?) OPTVAL=${OPTARG}; break ;;
  esac
done
# ...Remove the options processed
shift $((OPTIND - 1))
# ...Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed.
  \?) : ;;
  # Valid options, but missing arguments
  c) usage
     error_message "'-${OPTVAL}' option requires an argument"
     exit ${FAILURE} ;;
  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit ${FAILURE} ;;
esac


# Check the compiler option
compiler_exists ${COMPILER}
if [ $? -ne 0 ]; then
  error_message "Cannot find ${COMPILER} compiler"
  exit ${FAILURE}
fi


# =================
# Start the process
# =================

CURRENT_DIR=${PWD}

cd ${CRTM_SOURCE_ROOT}
if [ $? -ne 0 ]; then
  error_message "Error changing to CRTM source directory, ${CRTM_SOURCE_ROOT}"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

make create_links
if [ $? -ne 0 ]; then
  error_message "Error linking in source code and data"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

cd Build
if [ $? -ne 0 ]; then
  error_message "Error changing to CRTM build directory"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

. config-setup/${COMPILER}${DEBUG}.setup

NETCDF_DIR=${NETCDF} ./configure --prefix=${PWD}
if [ $? -ne 0 ]; then
  error_message "Error configuring the CRTM library build"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

make uninstall
if [ $? -ne 0 ]; then
  error_message "Error uninstalling prior to CRTM library build"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

make clean
if [ $? -ne 0 ]; then
  error_message "Error cleaning prior to CRTM library build"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

make
if [ $? -ne 0 ]; then
  error_message "Error building the CRTM library"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

make check
if [ $? -ne 0 ]; then
  error_message "Error checking the CRTM library build"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

make install
if [ $? -ne 0 ]; then
  error_message "Error installing the CRTM library"
  cd ${CURRENT_DIR}; exit ${FAILURE}
fi

# Return to original directory
cd ${CURRENT_DIR}
