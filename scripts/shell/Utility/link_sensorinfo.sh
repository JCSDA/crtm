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
  echo
}


usage()
{
  echo
  echo "Usage: link_sensorinfo.sh [-hx] [file]"
  echo
  echo "  Script to link in the master sensor information file, SensorInfo, from"
  echo "  the CRTM fixfile repository."
  echo
  echo '  If the optional [file] argument is not specified, the "SensorInfo" datafile'
  echo "  is linked in by default."
  echo
  echo
  echo " Options:"
  echo "  -h"
  echo "         Print this message."
  echo
  echo "  -x"
  echo "         Turn on execution tracing"
  echo
  echo " Optional arguments:"
  echo "   file"
  echo "         The particular sensor information to be linked. If not specified,"
  echo '         the "SensorInfo" file is linked by default.'
  echo  echo
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

# Set up
# ...Script name for error messages
SCRIPT_NAME=$(basename $0)
# ...Defintiions
SUCCESS=0; TRUE=0
FAILURE=1; FALSE=1
DEFAULT_FILE="SensorInfo"
# ...Define helper script, and make sure it can be found
LINK_SCRIPT="linkfiles.sh"
type ${LINK_SCRIPT} >/dev/null 2>&1 || {
  error_message "Cannot find ${LINK_SCRIPT} helper script. Exiting..."
  exit ${FAILURE}
}


# Parse the command line options
while getopts :hx OPTVAL; do
  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    h) usage | more; exit ${SUCCESS};;
    x) script_id; set -x;;
    \?) OPTVAL=${OPTARG}; break;;
  esac
done
# Remove the options processed
shift $(expr ${OPTIND} - 1)
# Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed
  \?) break;;
  # Invalid option
  ?) usage; error_message " Invalid option '-${OPTARG}'"; exit ${FAILURE};;
esac


# Check that the mandatory envar is defined
if [ -z "${CRTM_FIXFILE_ROOT}" ]; then
  error_message "CRTM_FIXFILE_ROOT envar is not defined."
  exit ${FAILURE}
fi


# Determine the file to link
# ...Set the default
SENSORINFO_FILE=${DEFAULT_FILE}
# ...Was one specified?
if [ $# -ne 0 ]; then
  SENSORINFO_FILE=$1
fi


# Output some user info
echo; echo "${SCRIPT_NAME}: Linking in the file ${SENSORINFO_FILE}"; echo


# Link in the file
if [ ! -f ${SENSORINFO_FILE} ]; then
  ${LINK_SCRIPT} -q ${CRTM_FIXFILE_ROOT} ${SENSORINFO_FILE}
  if [ $? -ne 0 ]; then
    error_message "Error occuring linking ${SENSORINFO_FILE} to ${PWD}/${SENSORINFO_FILE}"
    exit ${FAILURE}
  fi
else
  error_message "${SENSORINFO_FILE} file already exists in this directory!"
  exit ${FAILURE}
fi


# Done
exit ${SUCCESS}

