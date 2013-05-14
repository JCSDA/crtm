#!/bin/sh

# $Id$

usage()
{
  echo
  echo " Usage: run_SpcCoeff_NC2BIN.sh -l|b[fih]"
  echo
  echo "   Convert any netCDF format SpcCoeff files in the current"
  echo "   directory to Binary format. File with the suffix *.SpcCoeff.nc"
  echo "   are converted into *.SpcCoeff.bin files."
  echo
  echo " Options:"
  echo "   -l       Produce little-endian binary output files"
  echo
  echo "   -b       Produce big-endian binary output files"
  echo
  echo "   -f       Force overwrite of output file if it already exists."
  echo "            Default behaviour is to skip conversion if the output"
  echo "            file is already present."
  echo
  echo "   -i       Increment the output file data version number."
  echo "            Default behaviour is to not change the data version number."
  echo
  echo "   -h       Print this message and exit"
  echo
  echo " Comments:"
  echo "   Note the endian-ness option is only setup for those compilers"
  echo "   that allow for run-time conversion via environment variables."
  echo "   Check the 'endian_wrapper.sh' script help for a listing, i.e."
  echo
  echo "     $ endian_wrapper.sh -h"
  echo
}


error_message()
{
  SCRIPT_NAME="`basename $0`"
  MESSAGE=$1
  echo; echo "  ${SCRIPT_NAME}(ERROR): ${MESSAGE}"; echo
}


# Define defaults
ENDIAN_TYPE="NONE"
OVERWRITE="NO"
REMOVE="rm -f"
INCREMENT_VERSION="NO"


# Parse command line options
while getopts :hlbfi OPTVAL; do

  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    l)  ENDIAN_TYPE="little"; ENDIAN_ARG="l";;
    b)  ENDIAN_TYPE="big"; ENDIAN_ARG="b";;
    f)  OVERWRITE="YES";;
    i)  INCREMENT_VERSION="YES";;
    h)  usage; exit 0;;
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
        exit 1
      fi;;

  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit 1;;
esac


# Check that an endian type was specifed
if [ ${ENDIAN_TYPE} = "NONE" ]; then
  usage
  error_message "Must specify an endian type, -l or -b"
  exit 2
fi


# Assign processing parameters
EXE_FILE="SpcCoeff_NC2BIN"
LOG_FILE="${EXE_FILE}.${ENDIAN_TYPE}_endian.log"


# Process netCDF SpcCoeff files
for NC_FILE in `ls *.SpcCoeff.nc`; do

  # Create filenames
  BIN_FILE="`basename ${NC_FILE} .nc`.bin"
  SIGNAL_FILE="${BIN_FILE}.signal"

  # Check to see if the output file exists
  if [ -f ${BIN_FILE} -a ${OVERWRITE} = "NO" ]; then
    echo " Output file ${BIN_FILE} already exists. Skipping to next file..."
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
  
  # Convert the file
  echo " Converting ${NC_FILE} to ${ENDIAN_TYPE} endian binary format file ${BIN_FILE}..."
  endian_wrapper.sh -${ENDIAN_ARG} ${EXE_FILE} <<-NoMoreInput >> ${LOG_FILE}
	${NC_FILE}
	${BIN_FILE}
	${INCREMENT_VERSION}
	NoMoreInput

  # Check that signal file was created
  if [ -f ${SIGNAL_FILE} ]; then
    ${REMOVE} ${SIGNAL_FILE} > /dev/null
  else
    error_message "Error creating output file ${BIN_FILE}."
  fi

done
