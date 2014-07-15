#!/bin/sh

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
  echo " Usage: run_Create_SpcCoeff.sh [-fhx] [-v version]"
  echo
  echo "   Create netCDF SpcCoeff datafiles from any netCDF oSRF datafiles"
  echo "   that reside in the current directory."
  echo
  echo " Options:"
  echo "   -f"
  echo "         Force overwrite of output file if it already exists."
  echo "         Default behaviour is to skip conversion if the output"
  echo "         file is already present."
  echo
  echo "   -h"
  echo "         Print this message and exit"
  echo
  echo "   -x"
  echo "         Turn on execution tracing. This also causes extra script"
  echo "         information to be printed out."
  echo
  echo "   -v version"
  echo "         Use this option to assign a version number to the SpcCoeff"
  echo "         datafiles. If not specified, defaults to 1."
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



########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# Setup
# ...Script name for error messages
SCRIPT_NAME=$(basename $0)
# ...The executable name
EXE_FILE="Create_SpcCoeff"
# ...Defintiions
SUCCESS=0
FAILURE=1
# ...Define defaults
VERSION=1
OVERWRITE="NO"
REMOVE="rm -f"


# Parse command line options
while getopts :hfxv: OPTVAL; do

  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    v)  VERSION=${OPTARG} ;;
    x)  script_id; set -x ;;
    f)  OVERWRITE="YES";;
    h)  usage | more; exit ${SUCCESS} ;;
    \?) OPTVAL=${OPTARG}; break;;
  esac
done

# Remove the options processed
shift $((OPTIND - 1))

# Now output invalidities based on OPTVAL
# Need to do this as getopts does not handle
# the situations where an option is passed
# as an argument to another option.
case ${OPTVAL} in

  # If OPTVAL contains nothing, then all options
  # have been successfully parsed
  \?) if [ $# -ne 0 ]; then
        usage
        error_message "Invalid argument(s) $*"
        exit ${FAILURE}
      fi;;

  # Valid options, but missing arguments
  v) usage
     error_message "'-${OPTVAL}' option requires an argument"
     exit ${FAILURE} ;;

  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit ${FAILURE} ;;
esac


# Remove existing log file
LOG_FILE="${EXE_FILE}.log"
if [ -f ${LOG_FILE} ]; then
  ${REMOVE} ${LOG_FILE} > /dev/null
fi


# Process netCDF oSRF files
for OSRF_FILE in `ls *.osrf.nc`; do

  # Create filenames
  SPCCOEFF_FILE="`basename ${OSRF_FILE} .osrf.nc`.SpcCoeff.nc"
  SIGNAL_FILE="${SPCCOEFF_FILE}.signal"

  # Check to see if the output file exists
  if [ -f ${SPCCOEFF_FILE} -a ${OVERWRITE} = "NO" ]; then
    echo " Output file ${SPCCOEFF_FILE} already exists. Skipping to next file..."
    continue
  fi

  # Delete signal file if it exists
  if [ -f ${SIGNAL_FILE} ]; then
    ${REMOVE} ${SIGNAL_FILE}
    if [ $? -ne 0 ]; then
      error_message "Error deleting signal file ${SIGNAL_FILE}. Skipping to next file..."
      continue
    fi
  fi
  
  # Create the SpcCoeff file
  echo " Creating ${SPCCOEFF_FILE}..."
  ${EXE_FILE} <<-NoMoreInput >> ${LOG_FILE}
	${OSRF_FILE}
	${VERSION}
	NoMoreInput

  # Check that signal file was created
  if [ -f ${SIGNAL_FILE} ]; then
    ${REMOVE} ${SIGNAL_FILE} > /dev/null
  else
    error_message "Error creating output file ${SPCCOEFF_FILE}."
  fi

done
