#!/bin/sh

# --------------------------
# Usage description function
# --------------------------
usage()
{
  echo
  echo $1
  echo
  echo "   Shell script to display the values of the CRTM environment variables:"
  echo "     CRTM_SOURCE_ROOT    : The CRTM source code root directory"
  echo "     CRTM_FIXFILE_ROOT   : The CRTM fixed file root directory"
  echo "     CRTM_TEST_ROOT      : The CRTM test code root directory"
  echo "     CRTM_EXTERNALS_ROOT : The CRTM externals root directory"
  echo "     CRTM_SCRIPTS_ROOT   : The CRTM scripts root directory"
  echo "     CRTM_DOC_ROOT       : The CRTM documents root directory"
  echo "     CRTM_VALIDATION_ROOT: The CRTM validation root directory"
  echo
  echo "   NOTE: These envars are used in various scripts and makefiles within"
  echo "         the CRTM repository heirarchy so if you are going to change"
  echo "         them, make sure you know what you are doing!"
  echo
  echo
  echo "   Usage: crtm_env [-h] [-o [file]]"
  echo
  echo "   -o    Output envar settings to file for export. If no file is"
  echo '         specified, the default is $HOME/.crtm_env'
  echo
  echo "   -h    Print this message and exit"
  echo
}


# -----------------------------
# Error message output function
# -----------------------------
error_message()
{
  MESSAGE=$1
  echo; echo "${SCRIPT_NAME}(ERROR): ${MESSAGE}"
}


########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# ---------------------
# Setup and definitions
# ---------------------
# Version control info
RCSID='$Id$'

# The name of the script for message output
SCRIPT_NAME="`basename $0`"

# No output by default
OUTPUT=0
OUTPUT_DEFAULT="${HOME}/.crtm_env"


# ------------------------------
# Parse the command line options
# ------------------------------
while getopts :hxo: OPTVAL; do

  # If option argument looks like
  # another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    x)  set -x;;
    o)  OUTPUT=1; OUTPUT_FILE=${OPTARG};;
    h)  usage "${RCSID}"; exit 0;;
    :|\?) OPTVAL=${OPTARG}; break;;
  esac
done

# Remove the options processed
# ----------------------------
shift $((OPTIND - 1))

# Now output invalidities based on OPTVAL
# Need to do this as getopts does not handle
# the situations where an option is passed
# as an argument to another option.
# ------------------------------------------
case ${OPTVAL} in

  # If OPTVAL contains nothing, then all
  # options have been successfully parsed
  \?) :;;

  # Valid option, but missing argument
  o) OUTPUT=1
     OUTPUT_FILE=${OUTPUT_DEFAULT};;
                 
  # Invalid option
  ?) usage "${RCSID}"
     error_message "Invalid option '-${OPTARG}'"
     exit 1;;
esac


# ----------------------
# Create the output file
# ----------------------
if [ ${OUTPUT} -eq 1 ]; then
  echo "Creating output file ${OUTPUT_FILE}..."; echo
  echo "#" > ${OUTPUT_FILE}
  echo "# ${RCSID}" >> ${OUTPUT_FILE}
  echo "# CRTM environment variables. This file created by ${SCRIPT_NAME}" >> ${OUTPUT_FILE}
  echo "#" >> ${OUTPUT_FILE}
fi
  
  
# ------------------
# Display the envars
# ------------------
ENVAR_LIST="CRTM_SOURCE_ROOT CRTM_FIXFILE_ROOT CRTM_TEST_ROOT CRTM_EXTERNALS_ROOT CRTM_SCRIPTS_ROOT CRTM_DOC_ROOT CRTM_VALIDATION_ROOT"
for ENVAR in ${ENVAR_LIST}; do
  SETTING=`env | grep $ENVAR=`
  if [ -n "${SETTING}" ]; then
    echo ${SETTING}
    if [ ${OUTPUT} -eq 1 ]; then
      echo "export ${SETTING}" >> ${OUTPUT_FILE}
    fi
  fi
done


# ----
# Done
# ----
set +x
exit 0
