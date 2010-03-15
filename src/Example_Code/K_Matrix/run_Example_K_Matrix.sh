#!/bin/sh

# Check if the clean make target will be invoked.
CLEAN=0
if [ $# -eq 1 ]; then
  if [ "$1" = "clean" ]; then
    CLEAN=1
  fi
fi

# Initialise overall test counters
N_ALL_PASS=0
N_ALL_FAIL=0
N_ALL_TOTAL=0

# Loop over each executable
EXE_FILE_LIST="Example1_Simple Example2_SSU Example3_Zeeman Example4_ODPS"
for EXE_FILE in ${EXE_FILE_LIST}; do

  # Specify test sensor ids based on executables
  case "${EXE_FILE}" in
    "Example1_Simple") SENSOR_ID="amsua_metop-a mhs_n18 hirs4_n18 ssmis_f16 v.seviri_m09 amsre_aqua";;
    "Example2_SSU")    SENSOR_ID="ssu_n06 ssu_n14";;
    "Example3_Zeeman") SENSOR_ID="ssmis_f16";;
    "Example4_ODPS")   SENSOR_ID="amsua_metop-a mhs_n18 hirs4_n18 ssmis_f16 amsre_aqua";;
  esac    

  # Enter the current example directory
  cd ${EXE_FILE}
  
  # Build the executable
  make -s "EXE_FILE=${EXE_FILE}"
  if [ $? -ne 0 ]; then
    echo; echo "  Error building ${EXE_FILE} executable."; echo
    exit 1
  fi
  
  # Check that the example codes have been compiled
  # and are executable by the user.
  if [ ! -f ${EXE_FILE} ]; then
    echo; echo "  ${EXE_FILE} executable not found. Weird."; echo
    exit 1
  else
    if [ ! -x ${EXE_FILE} ]; then
      echo; echo "  ${EXE_FILE} found, but it does not have execute permission. That's even weirder."; echo
      exit 1
    fi
  fi

  # Initialise individual test counters
  N_PASS=0
  N_FAIL=0
  N_TOTAL=0

  # Loop over test sensors
  echo
  echo "----------------------------------------------"
  echo "Running K-matrix ${EXE_FILE} example"
  echo "  Sensors: ${SENSOR_ID}"
  echo "----------------------------------------------"
  for SID in ${SENSOR_ID}; do
    # Delete the sensor signal file
    SIGNALFILE="Results/${SID}.signal"
    if [ -f ${SIGNALFILE} ]; then
      rm -f ${SIGNALFILE}
    fi
    echo
    # Run the example code
    OUTFILE="${SID}.K_Matrix.output"
    ${EXE_FILE} <<-NoMoreInput > ${OUTFILE}
	${SID}
	NoMoreInput
    if [ $? -ne 0 ]; then
      echo "  Run failed for the ${SID} instrument!"
      ((N_FAIL = N_FAIL + 1))
      continue
    fi
    # Check the output results
    echo "Checking results for the ${SID} instrument..."
    if [ -f ${SIGNALFILE} ]; then
      echo "  The results are the same!"
      ((N_PASS = N_PASS + 1))
      rm -f ${SIGNALFILE} > /dev/null
    else
      echo "  The results are different! Check the output in ${OUTFILE}"
      ((N_FAIL = N_FAIL + 1))
    fi
    ((N_TOTAL = N_PASS + N_FAIL))
  done

  # Accumulate overall test results
  ((N_ALL_PASS = N_ALL_PASS + N_PASS))  
  ((N_ALL_FAIL = N_ALL_FAIL + N_FAIL))
  ((N_ALL_TOTAL = N_ALL_TOTAL + N_TOTAL))

  # Clean up
  if [ ${CLEAN} -eq 1 ]; then
    make -s clean "EXE_FILE=${EXE_FILE}"
    if [ $? -ne 0 ]; then
      echo; echo "  Error cleaning up after ${EXE_FILE} executable."; echo
      exit 1
    fi
  fi
  
  # Output individual test comnparison results
  if [ ${N_FAIL} -gt 0 ]; then
    WARNING="  <----<<<  **WARNING**"
  else
    WARNING=""
  fi
  echo
  echo "----------------------------------------------"
  echo "Summary of ${EXE_FILE} results"
  echo "----------------------------------------------"
  echo
  echo "Passed ${N_PASS} of ${N_TOTAL} tests."
  echo "Failed ${N_FAIL} of ${N_TOTAL} tests.${WARNING}"
  echo

  # Return to parent directory
  cd ..
done

# Output overall test comnparison results
if [ ${N_ALL_FAIL} -gt 0 ]; then
  WARNING="  <----<<<  **WARNING**"
else
  WARNING=""
fi
echo
echo "======================"
echo "SUMMARY OF ALL RESULTS"
echo "======================"
echo
echo "Passed ${N_ALL_PASS} of ${N_ALL_TOTAL} tests."
echo "Failed ${N_ALL_FAIL} of ${N_ALL_TOTAL} tests.${WARNING}"
echo

