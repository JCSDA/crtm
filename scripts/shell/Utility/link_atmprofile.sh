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
  echo "Usage: link_atmprofile.sh [-hx] [atm_id]"
  echo
  echo "  Script to link in the netCDF atmospheric profile datafile from the"
  echo "  CRTM fixfile repository."
  echo
  echo "  The valid atmospheric profile set identifiers are:"
  echo "    * UMBC48 : The UMBC 48-profile data set."
  echo "    * ECMWF52: The ECMWF 52-profile data set."
  echo "    * ECMWF83: The ECMWF 83-profile data set. [DEFAULT]"
  echo "    * ECMWF5K: The ECMWF 5000-profile data set."
  echo "    * CIMSS32: The CIMSS 32-profile data set."
  echo "    * Model6 : The Model climatology 6-profile data set."
  echo '  If the optional [atm_id] argument is not specified, the "ECMWF83" profile'
  echo "  set datafile is linked in by default."
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
  echo "  atm_id"
  echo "         The atmospheric profile ids identifying the profile data set to link in."
  echo '         If not specified, the "ECMWF83" data set is linked by default.'
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
SUCCESS=0; TRUE=0
FAILURE=1; FALSE=1
DEFAULT_ID="ECMWF83"
VALID_IDS="${DEFAULT_ID} UMBC48 ECMWF52 ECMWF5K CIMSS32 Model6"
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
# ...Set the default id
ATMPROFILE_ID=${DEFAULT_ID}
# ...Was one specified?
if [ $# -ne 0 ]; then
  ATMPROFILE_ID=$1
fi
# ...Is the id valid?
NO_MATCH=${TRUE}
for ID in ${VALID_IDS}; do
  if [ "${ID}" = "${ATMPROFILE_ID}" ]; then
    NO_MATCH=${FALSE}
    break
  fi
done
if [ ${NO_MATCH} -eq ${TRUE} ]; then
  error_message "AtmProfile id, ${ATMPROFILE_ID}, is not recognised."
  exit ${FAILURE}
fi
# ...Construct the filename
ATMPROFILE_FILE="${ATMPROFILE_ID}.AtmProfile.nc"


# Output some user info
echo; echo "${SCRIPT_NAME}: Linking in the file ${ATMPROFILE_FILE}"; echo


# Link in the file
if [ ! -f ${ATMPROFILE_FILE} ]; then
  ${LINK_SCRIPT} -q ${CRTM_FIXFILE_ROOT} ${ATMPROFILE_FILE}
  if [ $? -ne 0 ]; then
    error_message "Error occuring linking ${ATMPROFILE_FILE} to ${PWD}/${ATMPROFILE_FILE}"
    exit ${FAILURE}
  fi
else
  error_message "${ATMPROFILE_FILE} file already exists in this directory!"
  exit ${FAILURE}
fi


# Done
exit ${SUCCESS}

