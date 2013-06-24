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
  echo "Usage: link_srf.sh [-hx] sensor_id [sensor_id2 sensor_id3 ... sensor_idN]"
  echo
  echo "  Script to link in the spectral response function (SRF) for the requested"
  echo "  sensor from the CRTM SRF repository."
  echo
  echo
  echo " Options:"
  echo "  -h"
  echo "         Print this message."
  echo
  echo "  -x"
  echo "         Turn on execution tracing"
  echo
  echo " Arguments:"
  echo "  sensor_id [sensor_id2 sensor_id3 ... sensor_idN]"
  echo "         A list of sensor ids identifying the particular sensors for which"
  echo "         the SRF files are required."
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

# Set up
# ...Script name for error messages
SCRIPT_NAME=$(basename $0)
# ...Defintiions
SUCCESS=0
FAILURE=1
# ...Define helper script, and make sure it can be found
LINK_SCRIPT="linkfiles.sh"
type ${LINK_SCRIPT} >/dev/null 2>&1 || {
  error_message "Cannot find ${LINK_SCRIPT} helper script. Exiting..."
  exit ${FAILURE}
}


# Parse the command line options
while getopts :hx OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    h)  usage | more; exit ${SUCCESS};;
    x)  script_id; set -x;;
    \?) OPTVAL=${OPTARG}; break;;
  esac
done
# Remove the options processed
shift $(expr ${OPTIND} - 1)
# Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed and all that
  # remains are the arguments
  \?) if [ $# -lt 1 ]; then
        usage; error_message " Missing 'sensor_id' argument(s)"
        exit ${FAILURE}
      fi;;
  # Invalid option
  ?) usage; error_message " Invalid option '-${OPTARG}'"; exit ${FAILURE};;
esac


# Assign command line arguments
SENSOR_ID_LIST=$*


# Check that the mandatory envar is defined
if [ -z "${CRTM_SRF_ROOT}" ]; then
  error_message "CRTM_SRF_ROOT envar is not defined."
  exit ${FAILURE}
fi


# Construct the list of files to link
SRF_FILE_LIST=""
for SENSOR_ID in ${SENSOR_ID_LIST}; do
  SRF_FILE_LIST="${SRF_FILE_LIST} ${SENSOR_ID}.osrf.nc"
done
SRF_FILE_LIST=${SRF_FILE_LIST/[[:space:]]/}


# Output some user info
echo; echo "${SCRIPT_NAME}: Linking in the SRF files: ${SRF_FILE_LIST//[[:space:]]/, }"; echo

# And now link them in
${LINK_SCRIPT} -q ${CRTM_SRF_ROOT} ${SRF_FILE_LIST}
if [ $? -ne 0 ]; then
  error_message "Error occuring linking SRF datafiles}"
  exit ${FAILURE}
fi


# Done. Turn off execution tracing
exit ${SUCCESS}

