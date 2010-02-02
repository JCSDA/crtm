#!/bin/sh

usage()
{
  echo " Usage: run_ODPS_NC2BIN -l|b[fh]"
  echo
  echo "   Convert any netCDF format ODPS TauCoeff files in the current"
  echo "   directory to Binary format. File with the suffix *.TauCoeff.nc"
  echo "   are converted into *.TauCoeff.bin files."
  echo
  echo "    l       Produce little-endian binary output files"
  echo
  echo "    b       Produce big-endian binary output files"
  echo
  echo "    f       Force overwrite of output file if it already exists."
  echo "            Default behaviour is to skip conversion if the output"
  echo "            file is already present."
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


# Define defaults
ENDIAN_TYPE="NONE"
OVERWRITE="NO"


# Parse command line options
while getopts :hlbf OPTVAL; do

  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    l)  ENDIAN_TYPE="little";;
    b)  ENDIAN_TYPE="big";;
    f)  OVERWRITE="YES";;
    h)  usage
        exit 0;;
    :|\?) OPTVAL=${OPTARG}
          break;;
  esac

done

# Remove the options processed
shift `expr ${OPTIND} - 1`

# Now output invalidities based on OPTVAL
# Need to do this as getopts does not handle
# the situations where an option is passed
# as an argument to another option.
case ${OPTVAL} in

  # If OPTVAL contains nothing, then all options
  # have been successfully parsed
  \?) if [ $# -ne 0 ]; then
        ( echo " Invalid argument(s) $*" ; echo ; usage ) | more
        exit 2
      fi;;

  # Invalid option
  ?) ( echo " Invalid option '-${OPTARG}'" ; usage ) | more
     exit 2;;

esac

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
  *) ( echo " Must specify and endian type, -l or -b" ; echo ; usage ) | more
     exit 2;;
esac
# ...Switchable envars
export GFORTRAN_CONVERT_UNIT="${ENDIAN_TYPE}_endian"
export F_UFMTENDIAN="${ENDIAN_TYPE}"
export G95_ENDIAN="${ENDIAN_TYPE}"


# Assign processing parameters
EXE_FILE="ODPS_NC2BIN"
LOG_FILE="${EXE_FILE}.${ENDIAN_TYPE}_endian.log"


# Process netCDF TauCoeff files
for NC_FILE in `ls *.TauCoeff.nc`; do

  # Create binary filename
  BIN_FILE="`basename ${NC_FILE} .nc`.bin"

  # Check to see if the output file exists
  if [ -f ${BIN_FILE} -a ${OVERWRITE} = "NO" ]; then
    echo " Output file ${BIN_FILE} already exists. Skipping to next file..."
    continue
  fi

  # Convert the file
  echo " Converting ${NC_FILE} to ${ENDIAN_TYPE} endian binary format file ${BIN_FILE}..."
  ${EXE_FILE} <<-NoMoreInput >> ${LOG_FILE}
	${NC_FILE}
	${BIN_FILE}
	NoMoreInput
done


# Restore the run-time option environment variables
export XLFRTEOPTS="${XLFRTEOPTS_SAVE}"
export GFORTRAN_CONVERT_UNIT="${GFORTRAN_CONVERT_UNIT_SAVE}"
export F_UFMTENDIAN="${F_UFMTENDIAN_SAVE}"
export G95_ENDIAN="${G95_ENDIAN_SAVE}"
