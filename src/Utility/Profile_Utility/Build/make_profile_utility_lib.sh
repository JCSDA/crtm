#!/bin/sh
###############################################################################
#
# $Id$
#
# Script to configure, build, and install the library.
#
# The build configuration setup (compiler, compiler switched, libraries, etc)
# is specified via files in the config-setup/ subdirectory that are sourced
# within this script.
#
# The installation directory is ${PWD}
#
###############################################################################

usage()
{
  echo
  echo " Usage: make_profile_utility_lib.sh [-h] setup-file"
  echo
  echo "   Script to configure, build, test, and install the library."
  echo
  echo '   The installation directory is ${PWD}'
  echo
  echo " Options:"
  echo "   -h          Print this message and exit"
  echo
  echo " Arguments:"
  echo '   setup-file  File, in the "config-setup/" subdirectory, that contains'
  echo "               the build configuration setup (compiler, compiler switches,"
  echo "               libraries, etc) that are sourced within this script."
  echo
  echo "               Currently available setup files are:"
  for file in `ls ./config-setup/`; do
    echo "     `basename ${file}`" >&2
  done
  echo
}


# Setup
# ...Definitions
SCRIPT_NAME=$(basename $0)
SUCCESS=0
FAILURE=1


# Parse the command line options
while getopts :h OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    h)  usage
        exit ${SUCCESS};;
    :|\?) OPTVAL=${OPTARG}
          break;;
  esac
done
# ...Remove the options processed
shift $(expr ${OPTIND} - 1)
# ...Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed and all that
  # remains are the arguments
  \?) if [ $# -lt 1 ]; then
        echo; echo "${SCRIPT_NAME}: ERROR - Missing build setup argument"
        usage
        exit ${FAILURE}
      fi;;
  # Invalid option
  ?) echo "${SCRIPT_NAME}: ERROR - Invalid option '-${OPTARG}'"
     usage
     exit ${FAILURE};;
esac


# Source the build setup
SETUP_FILE="./config-setup/$1"
if [ ! -f ${SETUP_FILE} ]; then
  echo "${SCRIPT_NAME}: ERROR - Cannot find specified setup file ${SETUP_FILE}" >&2
  exit ${FAILURE}
fi
. ${SETUP_FILE}


# Generate the makefiles
echo; echo; echo; echo
echo "==============================================================="
echo "==============================================================="
echo "Configuring for build"
echo "==============================================================="
echo "==============================================================="
echo
./configure --prefix=${PWD}
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: Error configuring for build" >&2
  exit ${FAILURE}
fi

# Build the current configuration
echo; echo
echo "==============================================================="
echo "Starting build"
echo "==============================================================="
echo
make clean
make
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: Error building" >&2
  exit ${FAILURE}
fi

# Run tests for the current configuration
echo; echo
echo "==============================================================="
echo "Running tests"
echo "==============================================================="
echo
make check
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: ERROR running tests" >&2
  exit ${FAILURE}
fi

# Install the current build...
echo; echo
echo "==============================================================="
echo "Installing library"
echo "==============================================================="
echo
make install
if [ $? -ne 0 ]; then
  echo "${SCRIPT_NAME}: ERROR installing" >&2
  exit ${FAILURE}
fi

# Clean up
make distclean
