#!/bin/sh

# $Id$

usage()
{
  echo " Usage: endian_wrapper -l|-b [-h] exe_file"
  echo
  echo "   Run any Fortran executable for either little- or big-endian"
  echo "   unformatted file access regardless of how the executable was"
  echo "   originally built."
  echo
  echo "    l       Read and/or write little-endian binary output files"
  echo
  echo "    b       Read and/or write big-endian binary output files"
  echo
  echo "    h       Print this message and exit"
  echo
  echo "   Note the endian-ness option is only setup for the following"
  echo "   compilers that allow for run-time conversion via environment"
  echo "   variables:"
  echo "     - AIX xlf"
  echo "     - Linux gfortran"
  echo "     - Linux ifort (Intel)"
  echo "     - Linux g95"
  echo
}

error_message()
{
  MESSAGE=$1
  echo; echo "  ${SCRIPT_NAME}(ERROR): ${MESSAGE}"
}


########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# Define defaults
SCRIPT_NAME="`basename $0`"
ENDIAN_TYPE="NONE"
OVERWRITE="NO"
REMOVE="rm -f"


# Parse command line options
while getopts :hlb OPTVAL; do

  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    l)  ENDIAN_TYPE="little";;
    b)  ENDIAN_TYPE="big";;
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
  \?) if [ $# -lt 1 ]; then
        usage
        error_message "Missing 'exe_file' argument"
        exit 1
      fi;;

  # Invalid option
  ?) usage
     error_message " Invalid option '-${OPTARG}'"
     exit 1;;
esac

# Assign, and check, executable argument
INPUT_FILE=$1
EXE_FILE="`which ${INPUT_FILE}`"
if [ ! -f "${EXE_FILE}" ]; then
  error_message "${INPUT_FILE} is not a regular file"
  exit 2
fi
if [ ! -x "${EXE_FILE}" ]; then
  error_message "${INPUT_FILE} is not executable"
  exit 2
fi

# Check endian type for run time options
# ...Save the current envar options
XLFRTEOPTS_SAVE=${XLFRTEOPTS}
GFORTRAN_CONVERT_UNIT_SAVE=${GFORTRAN_CONVERT_UNIT}
F_UFMTENDIAN_SAVE=${F_UFMTENDIAN}
G95_ENDIAN_SAVE=${G95_ENDIAN}
# ...Set the non-switchable envars
case ${ENDIAN_TYPE} in
  "little") export XLFRTEOPTS="ufmt_littleendian=-100";;
  "big") export XLFRTEOPTS="";;
  *) usage; error_message "Must specify an endian type, -l or -b"
     exit 3;;
esac
# ...Switchable envars
export GFORTRAN_CONVERT_UNIT="${ENDIAN_TYPE}_endian"
export F_UFMTENDIAN="${ENDIAN_TYPE}"
export G95_ENDIAN="${ENDIAN_TYPE}"


# Assign processing parameters
LOG_FILE="${EXE_FILE}.${ENDIAN_TYPE}_endian.log"


# Run the executable
${EXE_FILE}


# Restore the run-time option environment variables
export XLFRTEOPTS="${XLFRTEOPTS_SAVE}"
export GFORTRAN_CONVERT_UNIT="${GFORTRAN_CONVERT_UNIT_SAVE}"
export F_UFMTENDIAN="${F_UFMTENDIAN_SAVE}"
export G95_ENDIAN="${G95_ENDIAN_SAVE}"
