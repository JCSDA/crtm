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
  echo "Usage: link_solar.sh [-hx] [file]"
  echo
  echo "  Script to link in the netCDF solar irradiance datafile from the"
  echo "  CRTM fixfile repository for creation of the instrument SpcCoeff"
  echo "  datafiles."
  echo
  echo "  The available datafiles for linking are:"
  echo "    * dF_0.1000.Solar.nc [DEFAULT]"
  echo "    * dF_0.0025.Solar.nc"
  echo "    * dF_0.0010.Solar.nc"
  echo '  If the optional [file] argument is not specified, the "dF_0.1000.Solar.nc"'
  echo "  datafile is linked in by default."
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
  echo "         The particular solar datafile, from the list above, to be linked."
  echo '         If not specified, the "dF_0.1000.Solar.nc" file is linked by default.'
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
DEFAULT_FILE="dF_0.1000.Solar.nc"
VALID_FILES="${DEFAULT_FILE} dF_0.0025.Solar.nc dF_0.0010.Solar.nc"
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
    h) usage | more; exit ${SUCCESS} ;;
    x) script_id; set -x ;;
    \?) OPTVAL=${OPTARG}; break ;;
  esac
done
# Remove the options processed
shift $(expr ${OPTIND} - 1)
# Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed
  \?) : ;;
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
SOLAR_FILE=${DEFAULT_FILE}
# ...Was one specified?
if [ $# -ne 0 ]; then
  SOLAR_FILE=$1
fi
# ...Is the filename valid?
NO_MATCH=${TRUE}
for FILE in ${VALID_FILES}; do
  if [ "${FILE}" = "${SOLAR_FILE}" ]; then
    NO_MATCH=${FALSE}
    break
  fi
done
if [ ${NO_MATCH} -eq ${TRUE} ]; then
  error_message "Solar irradiance datafile, ${SOLAR_FILE}, is not recognised."
  exit ${FAILURE}
fi


# Output some user info
echo; echo "${SCRIPT_NAME}: Linking in the file ${SOLAR_FILE}"; echo


# Link in the file
if [ ! -f ${SOLAR_FILE} ]; then
  ${LINK_SCRIPT} -q ${CRTM_FIXFILE_ROOT} ${SOLAR_FILE}
  if [ $? -ne 0 ]; then
    error_message "Error occuring linking ${SOLAR_FILE} to ${PWD}/${SOLAR_FILE}"
    exit ${FAILURE}
  fi
fi


# Done
exit ${SUCCESS}

