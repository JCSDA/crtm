#!/bin/bash

# Script to display the values of the CRTM environment variables
# in the current session.
#
# $Id$

script_id()
{
  REVISION='$Revision$'
  LAST_CHANGED_DATE='$LastChangedDate: $'
  echo
  echo "${SCRIPT_NAME} ${REVISION} ${LAST_CHANGED_DATE}"
  echo " "`date`
  echo " Support email: NCEP.List.EMC.JCSDA_CRTM.Support@noaa.gov"
}

usage()
{
  echo
  echo " Usage: crtm_env.sh [-hx] [-o [file]]"
  echo
  echo "   Script to display the values of the CRTM environment variables"
  echo "   in the current session."
  echo
  echo "   The CRTM envars displayed are:"
  echo "     CRTM_ROOT           : The CRTM working copy root directory"
  echo "     CRTM_SOURCE_ROOT    : The CRTM source code subdirectory"
  echo "     CRTM_FIXFILE_ROOT   : The CRTM fixed file subdirectory"
  echo "     CRTM_TEST_ROOT      : The CRTM test code subdirectory"
  echo "     CRTM_EXTERNALS_ROOT : The CRTM externals subdirectory"
  echo "     CRTM_SCRIPTS_ROOT   : The CRTM scripts subdirectory"
  echo "     CRTM_VALIDATION_ROOT: The CRTM validation subdirectory"
  echo "     CRTM_CONFIG_ROOT    : The CRTM configuration subdirectory"
  echo
  echo "   NOTE: These envars are used in various scripts and makefiles within"
  echo "         the CRTM repository heirarchy so if you are going to change"
  echo "         them, make sure you know what you are doing!"
  echo
  echo
  echo " Options:"
  echo "   -h"
  echo "         Print this message and exit"
  echo
  echo "   -x"
  echo "         Turn on execution tracing"
  echo
  echo "   -o [file]"
  echo "         Output envar settings to file for export. If no file is"
  echo '         specified, the default is $HOME/.crtm_env'
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


info_message()
{
  MESSAGE=$1
  echo "  ${SCRIPT_NAME}(INFORMATION): ${MESSAGE}"
}


########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# Set up
# ...Script name for error messages
SCRIPT_NAME=$(basename $BASH_SOURCE)
# ...Version number for output file
VERSION_ID='$Revision$'
# ...Definitions
SUCCESS=0; TRUE=0
FAILURE=1; FALSE=1
# ...Defaults
OUTPUT=${FALSE}
OUTPUT_FILE_DEFAULT="${HOME}/.crtm_env"


# Parse the command line options
while getopts :hxo: OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break ;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    h)  usage | more; exit ${SUCCESS} ;;
    x)  script_id; set -x ;;
    o)  OUTPUT=${TRUE}; OUTPUT_FILE=${OPTARG} ;;
    \?) OPTVAL=${OPTARG}; break ;;
  esac
done
# ...Remove the options processed
shift $((OPTIND - 1))
# ...Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed
  \?) : ;;
  # Valid option, but missing argument
  o) OUTPUT=${TRUE}
     OUTPUT_FILE=${OUTPUT_FILE_DEFAULT} ;;
  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit ${FAILURE} ;;
esac


# Create the output file
if [ ${OUTPUT} -eq ${TRUE} ]; then
  echo "Creating output file ${OUTPUT_FILE}..."; echo
  echo "#" > ${OUTPUT_FILE}
  echo "# This file created by ${SCRIPT_NAME} ${VERSION_ID}" >> ${OUTPUT_FILE}
  echo "#" >> ${OUTPUT_FILE}
  echo "# CRTM environment variables" >> ${OUTPUT_FILE}
  echo "#" >> ${OUTPUT_FILE}
fi
  
  
# Display the envars
ENVAR_LIST="CRTM_ROOT \
            CRTM_SOURCE_ROOT \
            CRTM_FIXFILE_ROOT \
            CRTM_TEST_ROOT \
            CRTM_EXTERNALS_ROOT \
            CRTM_SCRIPTS_ROOT \
            CRTM_VALIDATION_ROOT \
            CRTM_CONFIG_ROOT"
for ENVAR in ${ENVAR_LIST}; do
  SETTING=`env | grep $ENVAR=`
  if [ -n "${SETTING}" ]; then
    echo ${SETTING}
    if [ ${OUTPUT} -eq ${TRUE} ]; then
      echo "export ${SETTING}" >> ${OUTPUT_FILE}
    fi
  fi
done
