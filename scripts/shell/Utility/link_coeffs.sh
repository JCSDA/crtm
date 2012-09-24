#!/bin/sh

# Link script to link in CRTM release fixfiles into a single directory.
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
  echo " Usage: link_coeffs.sh [-xhla] source-dir dest-dir"
  echo
  echo "   Link in CRTM release fixfiles into a single directory."
  echo
  echo " Options:"
  echo "   -l"
  echo "         Link in little-endian files. BIG-ENDIAN is the default."
  echo
  echo "   -a"
  echo "         Link in ODAS TauCoeff files. ODPS is the default."
  echo
  echo "         Note: Currently there are no ODPS TauCoeff files"
  echo "               for visible sensors (we're working on it)."
  echo
  echo "   -x"
  echo "         Turn on execution tracing"
  echo
  echo "   -h"
  echo "         Print this message"
  echo
  echo " Arguments:"
  echo "   source-dir"
  echo "         The /fix directory location of the CRTM release."
  echo "         This directory must already exist."
  echo
  echo "   dest-dir"
  echo "         The directory into which the coefficients will be linked."
  echo "         If this directory does not exist, it is created."
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

script_id

# Setup
SCRIPT_NAME=`basename $0`

# ...Defintiions
SUCCESS=0
FAILURE=1

# ..Define defaults
ENDIAN_TYPE="Big_Endian"
TAUCOEFF_TYPE="ODPS"

# ...Define helper script, and make sure it can be found
LINK_SCRIPT="linkfiles.sh"
type ${LINK_SCRIPT} >/dev/null 2>&1 || {
  error_message "Cannot find ${LINK_SCRIPT} helper script. Exiting..."
  exit ${FAILURE}
}

# ...Define sensor independent coefficient files.
COMMON_COEFF_FILES="AerosolCoeff.bin \
CloudCoeff.bin \
FASTEM4.MWwater.EmisCoeff.bin \
FASTEM5.MWwater.EmisCoeff.bin \
Nalli.IRwater.EmisCoeff.bin \
WuSmith.IRwater.EmisCoeff.bin \
NPOESS.IRice.EmisCoeff.bin \
NPOESS.IRland.EmisCoeff.bin \
NPOESS.IRsnow.EmisCoeff.bin \
NPOESS.VISice.EmisCoeff.bin \
NPOESS.VISland.EmisCoeff.bin \
NPOESS.VISsnow.EmisCoeff.bin \
NPOESS.VISwater.EmisCoeff.bin \
IGBP.IRland.EmisCoeff.bin \
IGBP.VISland.EmisCoeff.bin \
USGS.IRland.EmisCoeff.bin \
USGS.VISland.EmisCoeff.bin"



# Parse command line options
while getopts :xhla OPTVAL; do

  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    l)  ENDIAN_TYPE="Little_Endian";;
    a)  TAUCOEFF_TYPE="ODAS";;
    x)  set -x;;
    h)  usage; exit ${SUCCESS};;
    \?) OPTVAL=${OPTARG}; break;;
  esac
done

# ...Remove the options processed
shift $((OPTIND - 1))

# ...Output invalidities based on OPTVAL
case ${OPTVAL} in

  # If OPTVAL contains nothing, then all options
  # have been successfully parsed.
  # So, check for presence of mandatory arguments.
  \?) if [ $# -lt 2 ]; then
        usage
        error_message "Missing source-dir and/or dest-dir arguments"
        exit ${FAILURE}
      fi;;

  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit ${FAILURE};;
esac



# Transfer the arguments
SOURCE_DIR=$1
DEST_DIR=$2



# Check the directory arguments

# ...Ensure the source directory exists
if [ ! -d ${SOURCE_DIR} ]; then
  usage
  error_message "Source directory '${SOURCE_DIR}' does not exist. Exiting."
  exit ${FAILURE}
fi

# ...Get the absolute path to the source directory
CURRENT_DIR=${PWD}
cd ${SOURCE_DIR}; SOURCE_DIR=`pwd -L`; cd ${CURRENT_DIR}

# ...Create the destination directory if necessary
if [ ! -d ${DEST_DIR} ]; then
  mkdir ${DEST_DIR}
  if [ $? -ne ${SUCCESS} ]; then
    error_message "Error creating destination directory '${DEST_DIR}'. Exiting."
    exit ${FAILURE}
  fi
fi



# Begin the linking process
echo
echo "Linking coefficient files from root source directory,"
echo "  ${SOURCE_DIR}"
echo "to destination directory,"
echo "  ${DEST_DIR}"

# ...Go to destination
cd ${DEST_DIR}
if [ $? -ne ${SUCCESS} ]; then
  error_message "Error cd'ing to destination directory '${DEST_DIR}'. Exiting."
  exit ${FAILURE}
fi

# ...Link common files
echo; echo "...linking sensor-independent coefficient files..."
${LINK_SCRIPT} -d ${ENDIAN_TYPE} ${SOURCE_DIR} ${COMMON_COEFF_FILES}

# ...Link the SpcCoeff files
echo; echo "...linking SpcCoeff coefficient files..."
SPCCOEFF_FILES=`ls ${SOURCE_DIR}/SpcCoeff/${ENDIAN_TYPE}`
${LINK_SCRIPT} -d ${ENDIAN_TYPE} ${SOURCE_DIR} ${SPCCOEFF_FILES}

# ...Link the TauCoeff files
echo; echo "...linking TauCoeff coefficient files..."
TAUCOEFF_FILES=`ls ${SOURCE_DIR}/TauCoeff/${TAUCOEFF_TYPE}/${ENDIAN_TYPE}`
${LINK_SCRIPT} -d ${ENDIAN_TYPE} ${SOURCE_DIR}/TauCoeff/${TAUCOEFF_TYPE} ${TAUCOEFF_FILES}

# ...Return to original directory
cd ${CURRENT_DIR}
