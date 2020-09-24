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
  echo " Usage: link_coeffs.sh [-achx] [-t file-type] source-dir dest-dir [sensor_id1 sensor id2 ... sensor_idN]"
  echo
  echo "   Link in CRTM release fixfiles into a single directory."
  echo
  echo " Options:"
  echo "   -a"
  echo "         Link in ODAS TauCoeff files. ODPS is the default."
  echo
  echo "         Note: Currently there are no ODPS TauCoeff files"
  echo "               for visible sensors (we're working on it)."
  echo
  echo "   -c"
  echo "         Perform additional special case linking for the AIRS 281 and"
  echo "         CrIS 399 channel subset datafiles for use with the GSI satinfo"
  echo "         file. The naming convention is:"
  echo "           airs281SUBSET_aqua.[Spc|Tau]Coeff.* -> airs281_aqua.[Spc|Tau]Coeff.*"
  echo "           cris_npp.[Spc|Tau]Coeff.*           -> cris399_aqua.[Spc|Tau]Coeff.*"
  echo
  echo "   -h"
  echo "         Print this message"
  echo
  echo "   -x"
  echo "         Turn on execution tracing"
  echo
  echo "   -t file-type"
  echo "         Use this option to specify the type of coefficient files to link."
  echo "         Valid targets are:"
  echo "           * big-endian [DEFAULT]"
  echo "           * little-endian"
  echo "           * netcdf"
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
  echo " Optional arguments:"
  echo "   sensor_id1 sensor id2 ... sensor_idN"
  echo "         A list of sensor ids identifying the particular sensors for which"
  echo "         the SpcCoeff and TauCoeff files are required. If not specified, all"
  echo "         available sensor files are linked."
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
SCRIPT_NAME=`basename $0`
# ...Defintiions
SUCCESS=0
FAILURE=1
# ..Define defaults
FILE_TYPE="big-endian"
TAUCOEFF_TYPE="ODPS"
ALL_SENSORS="yes"
SPECIAL_CASE="no"
# ...Define commands
LINK="ln -sf"
# ...Define helper script, and make sure it can be found
LINK_SCRIPT="linkfiles.sh"
type ${LINK_SCRIPT} >/dev/null 2>&1 || {
  error_message "Cannot find ${LINK_SCRIPT} helper script. Exiting..."
  exit ${FAILURE}
}



# Parse command line options
while getopts :achxt: OPTVAL; do
  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    a)  TAUCOEFF_TYPE="ODAS" ;;
    c)  SPECIAL_CASE="yes";;
    h)  usage | more; exit ${SUCCESS} ;;
    x)  script_id; set -x ;;
    t)  FILE_TYPE="${OPTARG}" ;;
    \?) OPTVAL=${OPTARG}; break ;;
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
  # Valid options, but missing arguments
  t) usage; error_message "'-${OPTVAL}' option requires an argument"; exit ${FAILURE} ;;
  # Invalid option
  ?) usage; error_message "Invalid option '-${OPTARG}'"; exit ${FAILURE};;
esac


# Check the file type option
case ${FILE_TYPE} in
  "big-endian")    FILE_DIR="Big_Endian"   ; FILE_EXT="bin" ;;
  "little-endian") FILE_DIR="Little_Endian"; FILE_EXT="bin" ;;
  "netcdf")        FILE_DIR="netCDF"       ; FILE_EXT="nc"  ;;
  "netcdf4")       FILE_DIR="netCDF"       ; FILE_EXT="nc4" ;;
  *) usage; error_message "Invalid file type option argument"; exit ${FAILURE} ;;
esac
# ...Define the sensor independent coefficient files.
COMMON_COEFF_FILES="AerosolCoeff.${FILE_EXT} \
IR.CloudCoeff.${FILE_EXT} \
MW.CloudCoeff.${FILE_EXT} \
CloudCoeff.${FILE_EXT} \
FASTEM4.MWwater.EmisCoeff.${FILE_EXT} \
FASTEM5.MWwater.EmisCoeff.${FILE_EXT} \
FASTEM6.MWwater.EmisCoeff.${FILE_EXT} \
Nalli.IRwater.EmisCoeff.${FILE_EXT} \
WuSmith.IRwater.EmisCoeff.${FILE_EXT} \
NPOESS.IRice.EmisCoeff.${FILE_EXT} \
NPOESS.IRland.EmisCoeff.${FILE_EXT} \
NPOESS.IRsnow.EmisCoeff.${FILE_EXT} \
NPOESS.VISice.EmisCoeff.${FILE_EXT} \
NPOESS.VISland.EmisCoeff.${FILE_EXT} \
NPOESS.VISsnow.EmisCoeff.${FILE_EXT} \
NPOESS.VISwater.EmisCoeff.${FILE_EXT} \
IGBP.IRland.EmisCoeff.${FILE_EXT} \
IGBP.VISland.EmisCoeff.${FILE_EXT} \
USGS.IRland.EmisCoeff.${FILE_EXT} \
USGS.VISland.EmisCoeff.${FILE_EXT}"


# Transfer, and remove, the arguments
SOURCE_DIR=$1
DEST_DIR=$2
shift 2


# Check if sensor ids have been specified
if [ $# -ne 0 ]; then
  SENSOR_ID_LIST="$*"
  ALL_SENSORS="no"
fi


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

# Go to destination
cd ${DEST_DIR}
if [ $? -ne ${SUCCESS} ]; then
  error_message "Error cd'ing to destination directory '${DEST_DIR}'. Exiting."
  exit ${FAILURE}
fi


# Link common files
echo; echo "...linking ${FILE_TYPE} sensor-independent coefficient files..."
${LINK_SCRIPT} -s -d ${FILE_DIR} ${SOURCE_DIR} ${COMMON_COEFF_FILES}


# Link the SpcCoeff files
echo; echo "...linking ${FILE_TYPE} SpcCoeff coefficient files..."
if [ "${ALL_SENSORS}" = "yes" ]; then
  SPCCOEFF_FILES=`ls ${SOURCE_DIR}/SpcCoeff/${FILE_DIR}`
else
  for SENSOR_ID in ${SENSOR_ID_LIST}; do
    SPCCOEFF_FILES="${SPCCOEFF_FILES} ${SENSOR_ID}.SpcCoeff.${FILE_EXT}"
  done
fi
${LINK_SCRIPT} -s -d ${FILE_DIR} ${SOURCE_DIR} ${SPCCOEFF_FILES}
# ...Perform special case linking if required
if [ "${SPECIAL_CASE}" = "yes" ]; then
  ${LINK} airs281_aqua.SpcCoeff.${FILE_EXT} airs281SUBSET_aqua.SpcCoeff.${FILE_EXT}
  ${LINK} cris399_npp.SpcCoeff.${FILE_EXT}  cris_npp.SpcCoeff.${FILE_EXT}
fi

# Link the TauCoeff files
echo; echo "...linking ${FILE_TYPE} TauCoeff coefficient files..."
if [ "${ALL_SENSORS}" = "yes" ]; then
  TAUCOEFF_FILES=`ls ${SOURCE_DIR}/TauCoeff/${TAUCOEFF_TYPE}/${FILE_DIR}`
else
  for SENSOR_ID in ${SENSOR_ID_LIST}; do
    TAUCOEFF_FILES="${TAUCOEFF_FILES} ${SENSOR_ID}.TauCoeff.${FILE_EXT}"
  done
fi
${LINK_SCRIPT} -s -d ${FILE_DIR} ${SOURCE_DIR}/TauCoeff/${TAUCOEFF_TYPE} ${TAUCOEFF_FILES}
# ...Perform special case linking if required
if [ "${SPECIAL_CASE}" = "yes" ]; then
  ${LINK} airs281_aqua.TauCoeff.${FILE_EXT} airs281SUBSET_aqua.TauCoeff.${FILE_EXT}
  ${LINK} cris399_npp.TauCoeff.${FILE_EXT}  cris_npp.TauCoeff.${FILE_EXT}
fi

# Return to original directory
cd ${CURRENT_DIR}
